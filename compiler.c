#include "compiler.h"

#include <stdint.h>

#include "bytecode.h"
#include "eval.h"
#include "memory.h"
#include "types.h"
#include "vm.h"

enum compile_res {
  COMP_SUCCESS = 0,
  COMP_FAILED,
};

struct compiler_ctx {
  /**
   * VM used for evaluating macros.
   */
  struct lisp_vm *vm;

  /**
   * Environment mirroring that of the form being compiled.
   *
   * - For macros, this will store the macro function for evluation in the
   * compile-time VM.
   * - For non-macro definitions, this will store `nil`. The value is unknown an
   * unimportant, but it needs to store something so that shadowing works the
   * same way during compilation as during runtime.
   */
  struct lisp_env *comp_env;

  /**
   * Current code chunk being compiled.
   */
  struct code_chunk *chunk;

  bool top_level;

  bool tail_pos;
};

static const struct lisp_symbol *SYMBOL_DO;
static const struct lisp_symbol *SYMBOL_IF;
static const struct lisp_symbol *SYMBOL_DEF;
static const struct lisp_symbol *SYMBOL_DEFMACRO;
static const struct lisp_symbol *SYMBOL_FN;
static const struct lisp_symbol *SYMBOL_QUOTE;

void init_global_compile_state(void) {
#define DEF_GLOBAL_SYM(var, name)                            \
  do {                                                       \
    /* Intermediate var because the global vars are const */ \
    struct lisp_symbol *s = lisp_symbol_create_cstr(name);   \
    var = s;                                                 \
    gc_push_root_obj(s);                                     \
  } while (false)

  // TODO Store these in the environment instead?
  // If builtins are given access to the environment they could be a sort of
  // builtin macros
  DEF_GLOBAL_SYM(SYMBOL_DO, "do");
  DEF_GLOBAL_SYM(SYMBOL_IF, "if");
  DEF_GLOBAL_SYM(SYMBOL_DEF, "def!");
  DEF_GLOBAL_SYM(SYMBOL_DEFMACRO, "defmacro!");
  DEF_GLOBAL_SYM(SYMBOL_FN, "fn");
  DEF_GLOBAL_SYM(SYMBOL_QUOTE, "quote");

#undef DEF_GLOBAL_SYM
}

static enum compile_res compile(struct compiler_ctx *ctx, struct lisp_val ast);

static void emit_byte(struct compiler_ctx *ctx, uint8_t byte) {
  chunk_append_byte(ctx->chunk, byte);
}

static void emit_instr(struct compiler_ctx *ctx, enum bytecode_op op) {
  emit_byte(ctx, op);
}

static enum compile_res emit_dup_fp(struct compiler_ctx *ctx,
                                    unsigned fp_offset) {
  if (fp_offset > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm, "stack frame too large");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_DUP_FP);
  emit_byte(ctx, fp_offset);
  return COMP_SUCCESS;
}

static void emit_return_if_tail_pos(struct compiler_ctx *ctx) {
  if (ctx->tail_pos) {
    emit_instr(ctx, OP_RETURN);
  }
}

static enum compile_res emit_const(struct compiler_ctx *ctx,
                                   struct lisp_val constant) {
  // TODO Check for existing constant
  unsigned const_idx = chunk_add_const(ctx->chunk, constant);
  if (const_idx > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm,
                              "too many constants in the current function");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_CONST);
  chunk_append_byte(ctx->chunk, const_idx);

  return COMP_SUCCESS;
}

static enum compile_res emit_call_instr(struct compiler_ctx *ctx,
                                        enum bytecode_op call_op,
                                        unsigned arg_count) {
  if (arg_count > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm, "too many arguments");
    return COMP_FAILED;
  }

  emit_instr(ctx, call_op);
  emit_byte(ctx, arg_count);
  return COMP_SUCCESS;
}

static unsigned emit_init_branch(struct compiler_ctx *ctx,
                                 enum bytecode_op branch_op) {
  chunk_append_byte(ctx->chunk, branch_op);
  return chunk_append_short(ctx->chunk, -1);
}

static enum compile_res fixup_branch_to_here(struct compiler_ctx *ctx,
                                             unsigned branch_code) {
  unsigned target = ctx->chunk->bytecode.size;
  int diff = target - branch_code;
  if (diff < INT16_MIN || INT16_MAX < diff) {
    vm_raise_format_exception(ctx->vm,
                              "branch target too far away: %d -> %d = %d",
                              branch_code, target, diff);
    return COMP_FAILED;
  }
  chunk_set_short(ctx->chunk, branch_code, diff);
  return COMP_SUCCESS;
}

static enum compile_res compile_constant(struct compiler_ctx *ctx,
                                         struct lisp_val constant) {
  enum compile_res res = emit_const(ctx, constant);
  emit_return_if_tail_pos(ctx);
  return res;
}

static enum compile_res compile_symbol_ref(struct compiler_ctx *ctx,
                                           struct lisp_symbol *sym) {
  enum compile_res res = emit_const(ctx, lisp_val_from_obj(sym));
  emit_instr(ctx, OP_LOOKUP);
  emit_return_if_tail_pos(ctx);
  return res;
}

static enum compile_res compile_do(struct compiler_ctx *ctx,
                                   struct lisp_val exprs) {
  if (lisp_val_is_nil(exprs)) {
    return compile_constant(ctx, LISP_VAL_NIL);
  }

  bool outer_tail_pos = ctx->tail_pos;
  // Intermediate expressions are never in tail position
  ctx->tail_pos = false;

  struct lisp_cons *exprs_cons;
  while ((exprs_cons = lisp_val_cast(LISP_CONS, exprs)) != NULL &&
         !lisp_val_is_nil(exprs_cons->cdr)) {
    enum compile_res res = compile(ctx, exprs_cons->car);
    if (res == COMP_FAILED) {
      return res;
    }

    // Ignore the result
    emit_instr(ctx, OP_POP);

    exprs = exprs_cons->cdr;
  }

  struct lisp_val last_expr = exprs_cons->car;

  ctx->tail_pos = outer_tail_pos;
  return compile(ctx, last_expr);
}

static enum compile_res compile_if(struct compiler_ctx *ctx,
                                   struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "if: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val cond_expr = args_cons->car;

  args_cons = lisp_val_cast(LISP_CONS, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "if: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val true_expr = args_cons->car;

  struct lisp_val false_expr;
  args_cons = lisp_val_cast(LISP_CONS, args_cons->cdr);
  if (args_cons == NULL) {
    false_expr = LISP_VAL_NIL;
  } else {
    false_expr = args_cons->car;

    if (!lisp_val_is_nil(args_cons->cdr)) {
      vm_raise_format_exception(ctx->vm, "if: too many args");
      return COMP_FAILED;
    }
  }

  bool outer_tail_pos = ctx->tail_pos;

  // Condition is never in tail position
  ctx->tail_pos = false;
  enum compile_res res = compile(ctx, cond_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  ctx->tail_pos = outer_tail_pos;

  unsigned false_branch_offset = emit_init_branch(ctx, OP_BRANCH_IF_FALSE);

  res = compile(ctx, true_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  // Only need a branch after the true code if it's not in tail position
  unsigned end_branch_offset = -1;
  if (!ctx->tail_pos) {
    end_branch_offset = emit_init_branch(ctx, OP_BRANCH);
  }

  res = fixup_branch_to_here(ctx, false_branch_offset);
  if (res == COMP_FAILED) {
    return res;
  }

  res = compile(ctx, false_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  if (!ctx->tail_pos) {
    res = fixup_branch_to_here(ctx, end_branch_offset);
    if (res == COMP_FAILED) {
      return res;
    }
  }

  return COMP_SUCCESS;
}

static enum compile_res parse_def(const char *label, struct compiler_ctx *ctx,
                                  struct lisp_val args,
                                  struct lisp_symbol **sym,
                                  struct lisp_val *value_expr) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "%s: not enough args", label);
    return COMP_FAILED;
  }
  *sym = lisp_val_cast(LISP_SYMBOL, args_cons->car);
  if (*sym == NULL) {
    vm_raise_format_exception(ctx->vm, "%s: first arg must be of type: %s",
                              lisp_type_name(LISP_SYMBOL));
    return COMP_FAILED;
  }

  args_cons = lisp_val_cast(LISP_CONS, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "%s: not enough args", label);
    return COMP_FAILED;
  }
  *value_expr = args_cons->car;

  if (!lisp_val_is_nil(args_cons->cdr)) {
    vm_raise_format_exception(ctx->vm, "%s: too many args", label);
    return COMP_FAILED;
  }

  return COMP_SUCCESS;
}

static enum compile_res compile_def(struct compiler_ctx *ctx,
                                    struct lisp_val args) {
  struct lisp_symbol *sym;
  struct lisp_val value_expr;
  enum compile_res res = parse_def("def!", ctx, args, &sym, &value_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  bool outer_tail_pos = ctx->tail_pos;
  ctx->tail_pos = false;
  res = compile(ctx, value_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  ctx->tail_pos = outer_tail_pos;

  // Copy the result so that it is also returned from the expression
  emit_instr(ctx, OP_DUP);

  res = emit_const(ctx, lisp_val_from_obj(sym));
  if (res == COMP_FAILED) {
    return res;
  }

  emit_instr(ctx, OP_BIND);

  emit_return_if_tail_pos(ctx);

  // Mark as NIL so that the variable can shadow macros and keywords
  lisp_env_set(ctx->comp_env, sym, LISP_VAL_NIL);
  return COMP_SUCCESS;
}

static enum compile_res compile_defmacro(struct compiler_ctx *ctx,
                                         struct lisp_val args) {
  struct lisp_symbol *sym;
  struct lisp_val value_expr;
  enum compile_res res = parse_def("defmacro!", ctx, args, &sym, &value_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  // New context for compiling the macro definition
  struct compiler_ctx macro_ctx = {
      .vm = ctx->vm,
      .comp_env = ctx->comp_env,
      .chunk = chunk_create(),
      .top_level = false,
      .tail_pos = true,
  };

  gc_push_root_obj(macro_ctx.chunk);

  res = compile(&macro_ctx, value_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  // Evaluate the macro expression in the compilation VM at the top-level
  enum eval_status eval_res = eval_chunk(ctx->vm, macro_ctx.chunk);
  if (eval_res == EV_EXCEPTION) {
    return COMP_FAILED;
  }

  gc_pop_root_expect_obj(macro_ctx.chunk);

  struct lisp_val macro_fn = vm_stack_pop(ctx->vm);
  if (!lisp_val_is_func(macro_fn)) {
    vm_raise_format_exception(ctx->vm, "defmacro!: value must be a function");
    return COMP_FAILED;
  }

  struct lisp_env *macro_env;
  if (ctx->top_level) {
    // Set in the VM environment so it persists
    macro_env = vm_global_env(ctx->vm);
  } else {
    // TODO Disallow local macros?
    macro_env = ctx->comp_env;
  }
  lisp_env_set_macro(macro_env, sym, macro_fn);

  return COMP_SUCCESS;
}

/**
 * Validate and generate binding code for function arguments.
 *
 * Outputs metadata through `req_arg_count` and `is_variadic`.
 */
static enum compile_res compile_fn_params(struct compiler_ctx *ctx,
                                          struct lisp_val raw_params,
                                          unsigned *req_arg_count,
                                          bool *is_variadic) {
  // TODO Process in reverse order to avoid copying values
  // TODO Leave arguments on the stack and do some sort of upvalue management
  enum compile_res res = COMP_SUCCESS;
  unsigned param_index = 0;
  while (lisp_val_type(raw_params) == LISP_CONS) {
    struct lisp_cons *cons_cell = lisp_val_as_obj(raw_params);
    struct lisp_val param_name = cons_cell->car;

    struct lisp_symbol *sym_name = lisp_val_cast(LISP_SYMBOL, param_name);
    if (sym_name == NULL) {
      vm_raise_format_exception(ctx->vm,
                                "fn: parameter names must be of type symbol");
      return COMP_FAILED;
    }

    // TODO Check for duplicate param names

    res = emit_dup_fp(ctx, param_index);
    if (res == COMP_FAILED) {
      return res;
    }

    res = emit_const(ctx, lisp_val_from_obj(sym_name));
    if (res == COMP_FAILED) {
      return res;
    }

    emit_instr(ctx, OP_BIND);

    raw_params = cons_cell->cdr;
    param_index++;
  }

  *req_arg_count = param_index;

  if (!lisp_val_is_nil(raw_params)) {
    *is_variadic = true;

    struct lisp_symbol *rest_param = lisp_val_cast(LISP_SYMBOL, raw_params);
    if (rest_param == NULL) {
      vm_raise_format_exception(ctx->vm,
                                "fn: parameter names must be of type symbol");
      return COMP_FAILED;
    }
    // TODO Check for duplicate param names

    emit_instr(ctx, OP_BUILD_REST_ARGS);
    emit_byte(ctx, param_index);
    res = emit_const(ctx, lisp_val_from_obj(rest_param));
    if (res == COMP_FAILED) {
      return res;
    }
    emit_instr(ctx, OP_BIND);
  } else {
    *is_variadic = false;
  }

  // Clean up the stack since the values are bound in the environment
  emit_instr(ctx, OP_CLEAR);

  return res;
}

static enum compile_res compile_fn(struct compiler_ctx *ctx,
                                   struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "fn: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val params = args_cons->car;

  args_cons = lisp_val_cast(LISP_CONS, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "fn: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val func_ast = args_cons->car;

  if (!lisp_val_is_nil(args_cons->cdr)) {
    vm_raise_format_exception(ctx->vm, "fn: too many args");
    return COMP_FAILED;
  }

  struct lisp_env *func_comp_env = lisp_env_create(ctx->comp_env);
  gc_push_root_obj(func_comp_env);
  struct code_chunk *func_code = chunk_create();
  gc_push_root_obj(func_code);

  struct compiler_ctx func_ctx = {
      .vm = ctx->vm,
      .comp_env = func_comp_env,
      .chunk = func_code,
      .top_level = false,
      .tail_pos = true,
  };

  unsigned req_arg_count;
  bool is_variadic;
  enum compile_res res =
      compile_fn_params(&func_ctx, params, &req_arg_count, &is_variadic);

  if (res == COMP_SUCCESS) {
    res = compile(&func_ctx, func_ast);
  }

  gc_pop_root_expect_obj(func_code);
  gc_pop_root_expect_obj(func_comp_env);

  if (res == COMP_FAILED) {
    return res;
  }

  // Back in the outer compiler context

  res = emit_const(ctx, lisp_val_from_obj(func_code));
  if (res == COMP_FAILED) {
    return res;
  }

  emit_instr(ctx, OP_MAKE_CLOSURE);
  emit_byte(ctx, req_arg_count);
  emit_byte(ctx, is_variadic);

  emit_return_if_tail_pos(ctx);
  return COMP_SUCCESS;
}

static enum compile_res compile_quote(struct compiler_ctx *ctx,
                                      struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "quote: not enough args");
    return COMP_FAILED;
  }

  if (!lisp_val_is_nil(args_cons->cdr)) {
    vm_raise_format_exception(ctx->vm, "quote: too many args");
    return COMP_FAILED;
  }

  struct lisp_val arg = args_cons->car;
  enum compile_res res = emit_const(ctx, arg);
  emit_return_if_tail_pos(ctx);
  return res;
}

static enum compile_res compile_func_call(struct compiler_ctx *ctx,
                                          struct lisp_val func_expr,
                                          struct lisp_val args_exprs) {
  // Function expression and arguments are not in tail position
  bool outer_tail_pos = ctx->tail_pos;
  ctx->tail_pos = false;

  // Evaluate args first so the function is on the top of the stack at the end
  enum compile_res res;
  unsigned arg_count = 0;
  while (!lisp_val_is_nil(args_exprs)) {
    struct lisp_cons *args_cons = lisp_val_as_obj(args_exprs);

    res = compile(ctx, args_cons->car);
    if (res == COMP_FAILED) {
      return res;
    }

    args_exprs = args_cons->cdr;
    arg_count++;
  }

  res = compile(ctx, func_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  ctx->tail_pos = outer_tail_pos;
  return emit_call_instr(ctx, ctx->tail_pos ? OP_TAIL_CALL : OP_CALL,
                         arg_count);
}

static enum compile_res expand_and_compile_macro(struct compiler_ctx *ctx,
                                                 struct lisp_val macro_fn,
                                                 struct lisp_val args) {
  enum eval_status eval_res = eval_apply(ctx->vm, macro_fn, args);
  if (eval_res == EV_EXCEPTION) {
    return COMP_FAILED;
  }

  struct lisp_val macro_output = vm_stack_pop(ctx->vm);
  return compile(ctx, macro_output);
}

static enum compile_res compile_call_or_special(struct compiler_ctx *ctx,
                                                struct lisp_cons *call_ast) {
  struct lisp_val head = call_ast->car;
  struct lisp_val args = call_ast->cdr;

  if (!lisp_val_is_list(args)) {
    vm_raise_format_exception(ctx->vm, "cannot compile improper list");
    return COMP_FAILED;
  }

  struct lisp_symbol *head_sym = lisp_val_cast(LISP_SYMBOL, head);
  if (head_sym != NULL) {
    const struct lisp_env_binding *binding =
        lisp_env_get(ctx->comp_env, head_sym);
    if (binding != NULL) {
      if (binding->is_macro) {
        return expand_and_compile_macro(ctx, binding->value, args);
      }
    } else {
      // See if it's a special form
      if (head_sym == SYMBOL_DO) {
        return compile_do(ctx, args);
      }
      if (head_sym == SYMBOL_IF) {
        return compile_if(ctx, args);
      }
      if (head_sym == SYMBOL_DEF) {
        return compile_def(ctx, args);
      }
      if (head_sym == SYMBOL_DEFMACRO) {
        return compile_defmacro(ctx, args);
      }
      if (head_sym == SYMBOL_FN) {
        return compile_fn(ctx, args);
      }
      if (head_sym == SYMBOL_QUOTE) {
        return compile_quote(ctx, args);
      }
    }
  }

  // Not a macro or special form, so it's a normal function application
  return compile_func_call(ctx, head, args);
}

/**
 * Compile AST into `current_code` in the context.
 *
 * In case of an error, return `NULL` and signal the exception in the
 * compiler's VM.
 */
static enum compile_res compile(struct compiler_ctx *ctx, struct lisp_val ast) {
  // TODO Make the compiler tail recursive?

  gc_push_root(ast);

  enum compile_res res;
  switch (lisp_val_type(ast)) {
    case LISP_NIL:
    case LISP_INT:
    case LISP_REAL:
    case LISP_CHAR:
    case LISP_STRING:
    case LISP_ATOM:
    case LISP_CLOSURE:
    case LISP_OPAQUE:
      res = compile_constant(ctx, ast);
      emit_return_if_tail_pos(ctx);
      break;

    case LISP_SYMBOL:
      res = compile_symbol_ref(ctx, lisp_val_as_obj(ast));
      break;

    case LISP_CONS:
      res = compile_call_or_special(ctx, lisp_val_as_obj(ast));
      break;

    default:
      vm_raise_format_exception(ctx->vm, "Cannot compile AST of type: %s",
                                lisp_val_type_name(ast));
      res = COMP_FAILED;
      break;
  }

  gc_pop_root_expect(ast);
  return res;
}

struct code_chunk *compile_top_level(struct lisp_vm *vm, struct lisp_val ast) {
  struct lisp_env *comp_env = lisp_env_create(vm_global_env(vm));
  gc_push_root_obj(comp_env);
  struct code_chunk *code = chunk_create();
  gc_push_root_obj(code);

  struct compiler_ctx ctx = {
      .vm = vm,
      .comp_env = comp_env,
      .chunk = code,
      .top_level = true,
      .tail_pos = true,
  };

  enum compile_res res = compile(&ctx, ast);
  gc_pop_root_expect_obj(code);
  gc_pop_root_expect_obj(comp_env);
  return res == COMP_SUCCESS ? code : NULL;
}

enum eval_status compile_eval(struct lisp_vm *vm, struct lisp_val ast) {
  struct code_chunk *code = compile_top_level(vm, ast);
  if (code == NULL) {
    return EV_EXCEPTION;
  }

  return eval_chunk(vm, code);
}
