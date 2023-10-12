#include "compiler.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#include "bytecode.h"
#include "core.h"
#include "eval.h"
#include "memory.h"
#include "types.h"
#include "vm.h"

enum compile_res {
  COMP_SUCCESS = 0,
  COMP_FAILED,
};

#define MAX_LOCALS UINT8_MAX

struct local_binding {
  struct lisp_symbol *sym;
  bool is_macro;
  bool is_boxed;
  union {
    struct lisp_val as_macro;
    uint8_t as_index;
  };
};

struct capture_binding {
  uint8_t index;
  bool is_local;
  bool is_boxed;
};

struct local_def {
  struct lisp_symbol *sym;
  struct lisp_val expr;
};

struct compiler_ctx {
  struct lisp_obj header;

  /**
   * VM used for evaluating macros.
   */
  struct lisp_vm *vm;

  /**
   * Containing compilation context.
   */
  struct compiler_ctx *outer;

  struct local_binding locals[MAX_LOCALS];
  uint8_t local_count;
  /**
   * Track the current stack frame size so that locals can be created in the
   * middle of the stack.
   */
  int current_stack_size;

  struct capture_binding captures[MAX_LOCALS];
  uint8_t capture_count;

  /**
   * Local `def!`s are collected and saved first, then code is generated in a
   * separate step so that they can be mutually recursive.
   */
  struct local_def local_defs[MAX_LOCALS];
  uint8_t local_def_count;

  /**
   * Current code chunk being compiled.
   */
  struct code_chunk *chunk;

  /**
   * Whether at the beginning of a new scope.
   */
  bool begin_pos;

  /**
   * Whether in tail position of the current scope.
   */
  bool tail_pos;

  /**
   * Whether at the top level scope.
   */
  bool top_level;

  /**
   * Name of the current binding.
   *
   * TODO Find a cleaner way to do this?
   * This is solely used for naming functions defined with `def!`.
   */
  struct lisp_symbol *binding_name;
};

static const struct lisp_symbol *SYMBOL_DO;
static const struct lisp_symbol *SYMBOL_IF;
static const struct lisp_symbol *SYMBOL_DEF;
static const struct lisp_symbol *SYMBOL_DEFSYNTAX;
static const struct lisp_symbol *SYMBOL_FN;
static const struct lisp_symbol *SYMBOL_LET;
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
  DEF_GLOBAL_SYM(SYMBOL_DEFSYNTAX, "defsyntax!");
  DEF_GLOBAL_SYM(SYMBOL_FN, "fn");
  DEF_GLOBAL_SYM(SYMBOL_LET, "let");
  DEF_GLOBAL_SYM(SYMBOL_QUOTE, "quote");

#undef DEF_GLOBAL_SYM
}

static void compiler_ctx_visit(struct lisp_val v, visit_callback cb,
                               void *cb_ctx) {
  const struct compiler_ctx *ctx = lisp_val_as_obj(v);
  cb(cb_ctx, lisp_val_from_obj(ctx->vm));
  if (ctx->outer != NULL) {
    cb(cb_ctx, lisp_val_from_obj(ctx->outer));
  }

  for (unsigned i = 0; i < ctx->local_count; i++) {
    cb(cb_ctx, lisp_val_from_obj(ctx->locals[i].sym));
    if (ctx->locals[i].is_macro) {
      cb(cb_ctx, ctx->locals[i].as_macro);
    }
  }

  for (unsigned i = 0; i < ctx->local_def_count; i++) {
    cb(cb_ctx, lisp_val_from_obj(ctx->local_defs[i].sym));
    cb(cb_ctx, ctx->local_defs[i].expr);
  }

  cb(cb_ctx, lisp_val_from_obj(ctx->chunk));
  cb(cb_ctx, lisp_val_from_obj(ctx->binding_name));
}

static void compiler_ctx_destroy(struct lisp_val v) { (void)v; }

static const struct lisp_vtable COMPILER_VTABLE = {
    .is_gc_managed = true,
    .name = "compiler-context",
    .visit_children = compiler_ctx_visit,
    .destroy = compiler_ctx_destroy,
};

static struct compiler_ctx *compiler_ctx_create_top_level(struct lisp_vm *vm) {
  struct code_chunk *new_chunk = chunk_create();
  gc_push_root_obj(new_chunk);

  struct compiler_ctx *ctx = lisp_obj_alloc(&COMPILER_VTABLE, sizeof(*ctx));
  ctx->vm = vm;
  ctx->outer = NULL;
  ctx->local_count = 0;
  ctx->current_stack_size = 0;
  ctx->capture_count = 0;
  ctx->local_def_count = 0;

  // There is no significance of `begin_pos` at the top-level
  ctx->begin_pos = false;
  ctx->tail_pos = true;
  ctx->top_level = true;
  ctx->binding_name = NULL;
  ctx->chunk = new_chunk;
  gc_pop_root_expect_obj(new_chunk);

  // Keep the value pushed until an explicit "complete"
  gc_push_root_obj(ctx);
  return ctx;
}

static struct compiler_ctx *compiler_ctx_create_inner(
    struct compiler_ctx *outer) {
  struct compiler_ctx *ctx = compiler_ctx_create_top_level(outer->vm);
  ctx->begin_pos = true;
  ctx->outer = outer;
  ctx->top_level = false;
  return ctx;
}

static struct code_chunk *compiler_ctx_complete(struct compiler_ctx *ctx) {
  gc_pop_root_expect_obj(ctx);
  return ctx->chunk;
}

static bool compiler_ctx_is_top_level(const struct compiler_ctx *ctx) {
  return ctx->top_level;
}

static enum compile_res compile(struct compiler_ctx *ctx, struct lisp_val ast);

static enum compile_res compile_non_tail(struct compiler_ctx *ctx,
                                         struct lisp_val ast) {
  bool outer_tail_pos = ctx->tail_pos;
  ctx->tail_pos = false;
  enum compile_res res = compile(ctx, ast);
  ctx->tail_pos = outer_tail_pos;
  return res;
}

static enum compile_res compile_non_top_level(struct compiler_ctx *ctx,
                                              struct lisp_val ast) {
  bool outer_top_level = ctx->top_level;
  ctx->top_level = false;
  enum compile_res res = compile(ctx, ast);
  ctx->top_level = outer_top_level;
  return res;
}

static struct lisp_closure *compile_chunk_to_closure(struct code_chunk *code) {
  return lisp_closure_create(code, 0);
}

static void inc_stack_size(struct compiler_ctx *ctx, int n) {
  ctx->current_stack_size += n;
  assert(ctx->current_stack_size >= 0);
}

static void emit_byte(struct compiler_ctx *ctx, uint8_t byte) {
  chunk_append_byte(ctx->chunk, byte);
}

static void emit_instr(struct compiler_ctx *ctx, enum bytecode_op op) {
  emit_byte(ctx, op);
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
  inc_stack_size(ctx, 1);

  return COMP_SUCCESS;
}

static void emit_pop(struct compiler_ctx *ctx) {
  emit_instr(ctx, OP_POP);
  inc_stack_size(ctx, -1);
}

static enum compile_res emit_create_uninitialized_box(
    struct compiler_ctx *ctx) {
  if (emit_const(ctx, lisp_uninitialized()) == COMP_FAILED) {
    return COMP_FAILED;
  }
  emit_instr(ctx, OP_INTRINSIC);
  // TODO Custom intrinsic for creating an uninitialized atom without the extra
  // `emit_const`?
  emit_byte(ctx, INTRINSIC_MAKE_ATOM);
  return COMP_SUCCESS;
}

static void emit_set_boxed_local(struct compiler_ctx *ctx) {
  emit_instr(ctx, OP_INTRINSIC);
  emit_byte(ctx, INTRINSIC_RESET);
  inc_stack_size(ctx, -1);

  // Ignore return from `reset!`
  emit_pop(ctx);
}

static void emit_unbox(struct compiler_ctx *ctx) {
  emit_instr(ctx, OP_INTRINSIC);
  emit_byte(ctx, INTRINSIC_DEREF);
}

static void emit_get_raw_local(struct compiler_ctx *ctx, uint8_t local_index) {
  const struct local_binding *b = &ctx->locals[local_index];
  assert(!b->is_macro);
  emit_instr(ctx, OP_GET_FP);
  emit_byte(ctx, b->as_index);
  inc_stack_size(ctx, 1);
}

static void emit_get_local(struct compiler_ctx *ctx, uint8_t local_index) {
  const struct local_binding *b = &ctx->locals[local_index];
  emit_get_raw_local(ctx, local_index);
  if (b->is_boxed) {
    emit_unbox(ctx);
  }
}

static void emit_get_raw_upvalue(struct compiler_ctx *ctx, uint8_t index) {
  emit_instr(ctx, OP_GET_UPVALUE);
  emit_byte(ctx, index);
  inc_stack_size(ctx, 1);
}

static void emit_get_upvalue(struct compiler_ctx *ctx, uint8_t index) {
  emit_get_raw_upvalue(ctx, index);
  if (ctx->captures[index].is_boxed) {
    emit_unbox(ctx);
  }
}

static enum compile_res emit_get_global(struct compiler_ctx *ctx,
                                        struct lisp_env_binding *binding) {
  // TODO Check for existing constant
  unsigned const_idx = chunk_add_const(ctx->chunk, lisp_val_from_obj(binding));
  if (const_idx > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm,
                              "too many constants in the current function");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_GET_GLOBAL);
  chunk_append_byte(ctx->chunk, const_idx);
  inc_stack_size(ctx, 1);

  return COMP_SUCCESS;
}

static enum compile_res emit_set_global(struct compiler_ctx *ctx,
                                        struct lisp_env_binding *binding) {
  // TODO Check for existing constant
  unsigned const_idx = chunk_add_const(ctx->chunk, lisp_val_from_obj(binding));
  if (const_idx > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm,
                              "too many constants in the current function");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_SET_GLOBAL);
  chunk_append_byte(ctx->chunk, const_idx);
  inc_stack_size(ctx, -1);

  return COMP_SUCCESS;
}

static void emit_return_if_tail_pos(struct compiler_ctx *ctx) {
  if (ctx->tail_pos) {
    emit_instr(ctx, OP_RETURN);
    ctx->current_stack_size = 0;
  }
}

static enum compile_res emit_alloc_closure(struct compiler_ctx *ctx,
                                           struct code_chunk *closure_code,
                                           uint8_t n_captures) {
  // TODO Check for existing constant
  unsigned const_idx =
      chunk_add_const(ctx->chunk, lisp_val_from_obj(closure_code));
  if (const_idx > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm,
                              "too many constants in the current function");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_ALLOC_CLOSURE);
  emit_byte(ctx, const_idx);
  emit_byte(ctx, n_captures);
  inc_stack_size(ctx, 1);
  return COMP_SUCCESS;
}

static void emit_init_closure(struct compiler_ctx *ctx, uint8_t n_captures) {
  emit_instr(ctx, OP_INIT_CLOSURE);
  emit_byte(ctx, n_captures);
  inc_stack_size(ctx, -(int)n_captures);
}

static enum compile_res emit_skip_delete(struct compiler_ctx *ctx,
                                         uint8_t skip_n, uint8_t delete_n) {
  if (skip_n + delete_n > ctx->current_stack_size) {
    vm_raise_format_exception(ctx->vm,
                              "tried to delete too many stack elements");
    return COMP_FAILED;
  }
  emit_instr(ctx, OP_SKIP_DELETE);
  emit_byte(ctx, skip_n);
  emit_byte(ctx, delete_n);
  inc_stack_size(ctx, -(int)delete_n);
  return COMP_SUCCESS;
}

static enum compile_res emit_call(struct compiler_ctx *ctx,
                                  unsigned arg_count) {
  if (arg_count > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm, "too many arguments");
    return COMP_FAILED;
  }

  emit_instr(ctx, OP_CALL);
  emit_byte(ctx, arg_count);
  // N*ARGS + 1*FUNCTION -> 1*RETURN
  inc_stack_size(ctx, -(int)arg_count);

  return COMP_SUCCESS;
}

static enum compile_res emit_tail_call(struct compiler_ctx *ctx,
                                       unsigned arg_count) {
  // Clear out the stack frame except for the arguments and the function
  unsigned remaining_stack_size = arg_count + 1;
  if (remaining_stack_size > UINT8_MAX) {
    vm_raise_format_exception(ctx->vm, "too many arguments");
    return COMP_FAILED;
  }

  enum compile_res res =
      emit_skip_delete(ctx, remaining_stack_size,
                       ctx->current_stack_size - remaining_stack_size);
  if (res == COMP_FAILED) {
    return res;
  }

  emit_instr(ctx, OP_TAIL_CALL);
  ctx->current_stack_size = arg_count;
  return COMP_SUCCESS;
}

static unsigned emit_init_branch(struct compiler_ctx *ctx) {
  chunk_append_byte(ctx->chunk, OP_BRANCH);
  return chunk_append_short(ctx->chunk, -1);
}

static unsigned emit_init_branch_if_false(struct compiler_ctx *ctx) {
  chunk_append_byte(ctx->chunk, OP_BRANCH_IF_FALSE);
  inc_stack_size(ctx, -1);
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

static struct local_binding *compiler_lookup_local_after(
    struct compiler_ctx *ctx, struct lisp_symbol *sym, uint8_t starting_local) {
  // Search in reverse so later bindings take priority
  for (int i = (int)ctx->local_count - 1; i >= starting_local; i--) {
    if (lisp_symbol_eq(ctx->locals[i].sym, sym)) {
      return &ctx->locals[i];
    }
  }
  return NULL;
}

static struct local_binding *compiler_lookup_local(struct compiler_ctx *ctx,
                                                   struct lisp_symbol *sym) {
  return compiler_lookup_local_after(ctx, sym, 0);
}

static struct local_binding *compiler_alloc_local(struct compiler_ctx *ctx,
                                                  struct lisp_symbol *sym) {
  if (ctx->local_count == MAX_LOCALS) {
    vm_raise_format_exception(ctx->vm, "too many local bindings");
    return NULL;
  }

  struct local_binding *created = &ctx->locals[ctx->local_count++];
  *created = (struct local_binding){
      .sym = sym,
  };
  return created;
}

static enum compile_res compiler_add_param(struct compiler_ctx *ctx,
                                           struct lisp_symbol *sym) {
  if (compiler_lookup_local(ctx, sym) != NULL) {
    vm_raise_format_exception(ctx->vm, "duplicate parameter binding: %s",
                              lisp_symbol_name(sym));
    return COMP_FAILED;
  }

  struct local_binding *b = compiler_alloc_local(ctx, sym);
  if (b == NULL) {
    return COMP_FAILED;
  }

  b->is_macro = false;
  b->is_boxed = false;
  b->as_index = ctx->current_stack_size++;
  return COMP_SUCCESS;
}

static enum compile_res compiler_add_local(struct compiler_ctx *ctx,
                                           struct lisp_symbol *sym,
                                           int stack_index) {
  struct local_binding *b = compiler_alloc_local(ctx, sym);
  if (b == NULL) {
    return COMP_FAILED;
  }

  b->is_macro = false;
  b->is_boxed = false;
  b->as_index = stack_index;
  return COMP_SUCCESS;
}

static enum compile_res compiler_add_boxed_local(struct compiler_ctx *ctx,
                                                 struct lisp_symbol *sym) {
  int stack_index = ctx->current_stack_size;
  if (stack_index > UINT8_MAX) {
    return COMP_FAILED;
  }

  if (emit_create_uninitialized_box(ctx) == COMP_FAILED) {
    return COMP_FAILED;
  }

  struct local_binding *b = compiler_alloc_local(ctx, sym);
  if (b == NULL) {
    return COMP_FAILED;
  }

  b->is_macro = false;
  b->is_boxed = true;
  b->as_index = stack_index;
  return COMP_SUCCESS;
}

static enum compile_res compiler_add_local_macro(struct compiler_ctx *ctx,
                                                 struct lisp_symbol *sym,
                                                 struct lisp_val macro_fn) {
  struct local_binding *b = compiler_alloc_local(ctx, sym);
  if (b == NULL) {
    return COMP_FAILED;
  }

  b->is_macro = true;
  b->is_boxed = false;
  b->as_macro = macro_fn;
  return COMP_SUCCESS;
}

static enum compile_res compile_constant(struct compiler_ctx *ctx,
                                         struct lisp_val constant) {
  enum compile_res res = emit_const(ctx, constant);
  emit_return_if_tail_pos(ctx);
  return res;
}

static enum compile_res add_upvalue(struct compiler_ctx *ctx,
                                    uint8_t parent_index, bool is_local,
                                    bool is_boxed, uint8_t *created_index) {
  for (unsigned i = 0; i < ctx->capture_count; i++) {
    struct capture_binding *b = &ctx->captures[i];
    if (b->index == parent_index && b->is_local == is_local) {
      *created_index = i;
      return COMP_SUCCESS;
    }
  }

  if (ctx->capture_count >= MAX_LOCALS) {
    vm_raise_format_exception(ctx->vm, "too many captured upvalues");
    return COMP_FAILED;
  }

  ctx->captures[ctx->capture_count] = (struct capture_binding){
      .index = parent_index,
      .is_local = is_local,
      .is_boxed = is_boxed,
  };
  *created_index = ctx->capture_count++;
  return COMP_SUCCESS;
}

enum symbol_type {
  SYM_LOCAL,
  SYM_CAPTURE,
  SYM_GLOBAL,
  SYM_MACRO,
  SYM_UNBOUND,
};

struct resolved_sym {
  enum symbol_type type;
  union {
    uint8_t index;
    struct lisp_val macro_fn;
    struct lisp_env_binding *global_binding;
  } value;
};

static void resolve_global_type(struct compiler_ctx *ctx,
                                struct lisp_symbol *sym,
                                struct resolved_sym *resolved) {
  struct lisp_env_binding *binding = lisp_env_get(ctx->vm->global_env, sym);
  if (binding == NULL) {
    resolved->type = SYM_UNBOUND;
  } else if (lisp_env_binding_is_macro(binding)) {
    resolved->type = SYM_MACRO;
    resolved->value.macro_fn = lisp_env_binding_value(binding);
  } else {
    resolved->type = SYM_GLOBAL;
    resolved->value.global_binding = binding;
  }
}

/**
 * Scan through current lexical scope to find where a symbol is defined.
 *
 * If the symbol is captured from a parent scope, the necessary capture bindings
 * are made.
 */
static enum compile_res resolve_symbol(struct compiler_ctx *ctx,
                                       struct lisp_symbol *sym,
                                       struct resolved_sym *resolved) {
  const struct local_binding *local = compiler_lookup_local(ctx, sym);
  if (local != NULL) {
    if (local->is_macro) {
      resolved->type = SYM_MACRO;
      resolved->value.macro_fn = local->as_macro;
    } else {
      resolved->type = SYM_LOCAL;
      // TODO Cleaner way to get the index?
      resolved->value.index = local - ctx->locals;
    }
    return COMP_SUCCESS;
  }

  if (ctx->outer == NULL) {
    resolve_global_type(ctx, sym, resolved);
    return COMP_SUCCESS;
  }

  enum compile_res res = resolve_symbol(ctx->outer, sym, resolved);
  if (res == COMP_FAILED) {
    return res;
  }

  switch (resolved->type) {
    case SYM_LOCAL: {
      uint8_t parent_index = resolved->value.index;
      resolved->type = SYM_CAPTURE;
      return add_upvalue(ctx, parent_index, true,
                         ctx->outer->locals[parent_index].is_boxed,
                         &resolved->value.index);
    }
    case SYM_CAPTURE: {
      uint8_t parent_index = resolved->value.index;
      // "Translate" the parent capture into a capture for the current context
      return add_upvalue(ctx, parent_index, false,
                         ctx->outer->captures[parent_index].is_boxed,
                         &resolved->value.index);
    }
    case SYM_GLOBAL:
    case SYM_MACRO:
    case SYM_UNBOUND:
      return COMP_SUCCESS;
  }

  assert(false);
  return COMP_FAILED;
}

/**
 * Generate code for all `def!`s in the current local scope. Called before
 * compiling any non-def! form.
 *
 * For simplicity, all local `def!`s generate boxed locals (since they may be
 * mutually recursive).
 */
static enum compile_res compile_prepare_non_def(struct compiler_ctx *ctx) {
  if (!ctx->begin_pos) {
    return COMP_SUCCESS;
  }

  ctx->begin_pos = false;
  bool outer_tail_pos = ctx->tail_pos;
  ctx->tail_pos = false;

  enum compile_res res = COMP_SUCCESS;

  unsigned starting_local_index = ctx->local_count;

  // Pre-allocate boxes, but don't initialize
  for (unsigned i = 0; i < ctx->local_def_count; i++) {
    struct lisp_symbol *sym = ctx->local_defs[i].sym;
    res = compiler_add_boxed_local(ctx, sym);
    if (res == COMP_FAILED) {
      return res;
    }
  }

  // Initialize each in order
  for (unsigned i = 0; i < ctx->local_def_count; i++) {
    emit_get_raw_local(ctx, starting_local_index + i);
    ctx->binding_name = ctx->local_defs[i].sym;
    res = compile(ctx, ctx->local_defs[i].expr);
    ctx->binding_name = NULL;
    if (res == COMP_FAILED) {
      return res;
    }
    emit_set_boxed_local(ctx);
  }

  ctx->tail_pos = outer_tail_pos;
  ctx->local_def_count = 0;
  return COMP_SUCCESS;
}

static enum compile_res compile_symbol_ref(struct compiler_ctx *ctx,
                                           struct lisp_symbol *sym) {
  struct resolved_sym resolved;
  resolve_symbol(ctx, sym, &resolved);
  switch (resolved.type) {
    case SYM_LOCAL:
      emit_get_local(ctx, resolved.value.index);
      break;
    case SYM_CAPTURE:
      emit_get_upvalue(ctx, resolved.value.index);
      break;
    case SYM_UNBOUND:
      resolved.value.global_binding =
          lisp_env_set(ctx->vm->global_env, sym, lisp_uninitialized());
      // Fallthough to global case now that the binding is created
      __attribute__((fallthrough));
    case SYM_GLOBAL:
      if (emit_get_global(ctx, resolved.value.global_binding) == COMP_FAILED) {
        return COMP_FAILED;
      }
      break;
    case SYM_MACRO:
      vm_raise_format_exception(
          ctx->vm, "cannot resolve macro symbol '%s' in non-macro context",
          lisp_symbol_name(sym));
      return COMP_FAILED;
  }

  emit_return_if_tail_pos(ctx);
  return COMP_SUCCESS;
}

static enum compile_res expand_macro(struct compiler_ctx *ctx,
                                     struct lisp_val macro_fn,
                                     struct lisp_val args,
                                     struct lisp_val *expanded_ast) {
  enum eval_status eval_res = eval_apply(ctx->vm, macro_fn, args);
  if (eval_res == EV_EXCEPTION) {
    return COMP_FAILED;
  }

  *expanded_ast = vm_stack_pop(ctx->vm);
  return COMP_SUCCESS;
}

static enum compile_res expand_and_compile_macro(struct compiler_ctx *ctx,
                                                 struct lisp_val macro_fn,
                                                 struct lisp_val args) {
  struct lisp_val expanded;
  if (expand_macro(ctx, macro_fn, args, &expanded) == COMP_FAILED) {
    return COMP_FAILED;
  }
  return compile(ctx, expanded);
}

static enum compile_res compile_do(struct compiler_ctx *ctx,
                                   struct lisp_val exprs) {
  if (lisp_val_is_nil(exprs)) {
    if (ctx->tail_pos) {
      // Skip this if in non-tail-pos so that an empty `(do)` can be between
      // `def!`s
      compile_prepare_non_def(ctx);
    }
    return compile_constant(ctx, LISP_VAL_NIL);
  }

  bool outer_tail_pos = ctx->tail_pos;
  // Intermediate expressions are never in tail position
  ctx->tail_pos = false;

  struct lisp_cons *exprs_cons;
  while ((exprs_cons = lisp_val_cast(lisp_val_is_cons, exprs)) != NULL &&
         !lisp_val_is_nil(exprs_cons->cdr)) {
    enum compile_res res = compile(ctx, exprs_cons->car);
    if (res == COMP_FAILED) {
      return res;
    }

    // Ignore the result
    emit_pop(ctx);

    exprs = exprs_cons->cdr;
  }

  struct lisp_val last_expr = exprs_cons->car;

  ctx->tail_pos = outer_tail_pos;
  return compile(ctx, last_expr);
}

static enum compile_res compile_if(struct compiler_ctx *ctx,
                                   struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "if: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val cond_expr = args_cons->car;

  args_cons = lisp_val_cast(lisp_val_is_cons, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "if: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val true_expr = args_cons->car;

  struct lisp_val false_expr;
  args_cons = lisp_val_cast(lisp_val_is_cons, args_cons->cdr);
  if (args_cons == NULL) {
    false_expr = LISP_VAL_NIL;
  } else {
    false_expr = args_cons->car;

    if (!lisp_val_is_nil(args_cons->cdr)) {
      vm_raise_format_exception(ctx->vm, "if: too many args");
      return COMP_FAILED;
    }
  }

  // Condition is never in tail position
  enum compile_res res = compile_non_tail(ctx, cond_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  unsigned false_branch_offset = emit_init_branch_if_false(ctx);
  // Save the stack size since it needs to be restored for the other branch
  int branch_stack_size = ctx->current_stack_size;

  res = compile(ctx, true_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  // Only need a branch after the true code if it's not in tail position
  unsigned end_branch_offset = -1;
  if (!ctx->tail_pos) {
    end_branch_offset = emit_init_branch(ctx);
  }

  res = fixup_branch_to_here(ctx, false_branch_offset);
  if (res == COMP_FAILED) {
    return res;
  }

  ctx->current_stack_size = branch_stack_size;
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
  struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "%s: not enough args", label);
    return COMP_FAILED;
  }
  *sym = lisp_val_cast(lisp_val_is_symbol, args_cons->car);
  if (*sym == NULL) {
    vm_raise_format_exception(ctx->vm, "%s: first arg must be of type symbol",
                              label);
    return COMP_FAILED;
  }

  args_cons = lisp_val_cast(lisp_val_is_cons, args_cons->cdr);
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

static enum compile_res compile_top_level_def(struct compiler_ctx *ctx,
                                              struct lisp_symbol *sym,
                                              struct lisp_val expr) {
  // Create the binding if it doesn't already exist so that the binding can
  // shadow macros and keywords
  struct resolved_sym resolved;
  resolve_global_type(ctx, sym, &resolved);
  if (resolved.type != SYM_GLOBAL) {
    // Either SYM_MACRO or SYM_UNBOUND
    // TODO Special flag in lisp_env which indicates uninitialized?
    resolved.value.global_binding =
        lisp_env_set(ctx->vm->global_env, sym, lisp_uninitialized());
  }

  ctx->binding_name = sym;
  enum compile_res res = compile_non_tail(ctx, expr);
  ctx->binding_name = NULL;

  if (res == COMP_FAILED) {
    return res;
  }

  res = emit_set_global(ctx, resolved.value.global_binding);
  if (res == COMP_FAILED) {
    return res;
  }

  // Return value
  emit_const(ctx, lisp_non_printing());
  emit_return_if_tail_pos(ctx);
  return COMP_SUCCESS;
}

static enum compile_res compile_local_def(struct compiler_ctx *ctx,
                                          struct lisp_symbol *sym,
                                          struct lisp_val expr) {
  if (!ctx->begin_pos) {
    vm_raise_format_exception(ctx->vm,
                              "def!: must occur at beginning of local scope");
    return COMP_FAILED;
  }

  if (ctx->local_def_count >= MAX_LOCALS) {
    vm_raise_format_exception(ctx->vm, "def!: too many local definitions");
    return COMP_FAILED;
  }

  ctx->local_defs[ctx->local_def_count++] = (struct local_def){
      .sym = sym,
      .expr = expr,
  };

  // Still need to return a value so that the stack is in the proper state.
  // Probably the only way to avoid it would be a separate pass to collect a
  // strip out `def!`s.
  emit_const(ctx, lisp_non_printing());
  emit_return_if_tail_pos(ctx);
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

  if (compiler_ctx_is_top_level(ctx)) {
    return compile_top_level_def(ctx, sym, value_expr);
  } else {
    return compile_local_def(ctx, sym, value_expr);
  }
}

static enum compile_res compile_defsyntax(struct compiler_ctx *ctx,
                                          struct lisp_val args) {
  struct lisp_symbol *sym;
  struct lisp_val value_expr;
  enum compile_res res = parse_def("defsyntax!", ctx, args, &sym, &value_expr);
  if (res == COMP_FAILED) {
    return res;
  }

  // New context for compiling the macro definition
  struct compiler_ctx *macro_ctx = compiler_ctx_create_inner(ctx);
  ctx->binding_name = sym;

  res = compile(macro_ctx, value_expr);
  struct code_chunk *func = compiler_ctx_complete(macro_ctx);

  if (res == COMP_FAILED) {
    return res;
  }

  // Evaluate the macro expression in the compilation VM at the top-level
  enum eval_status eval_res =
      eval_closure(ctx->vm, compile_chunk_to_closure(func));
  if (eval_res == EV_EXCEPTION) {
    return COMP_FAILED;
  }

  struct lisp_val macro_fn = vm_stack_pop(ctx->vm);
  if (!lisp_val_is_func(macro_fn)) {
    vm_raise_format_exception(ctx->vm, "defsyntax!: value must be a function");
    return COMP_FAILED;
  }

  if (compiler_ctx_is_top_level(ctx)) {
    // Set in the VM environment so it persists
    lisp_env_set_macro(ctx->vm->global_env, sym, macro_fn);
  } else {
    if (compiler_add_local_macro(ctx, sym, macro_fn) == COMP_FAILED) {
      return COMP_FAILED;
    }
  }

  // Need to emit something for the produced code
  emit_const(ctx, lisp_non_printing());
  emit_return_if_tail_pos(ctx);
  return COMP_SUCCESS;
}

/**
 * Validate and generate binding code for function arguments.
 *
 * Outputs metadata through `req_arg_count` and `is_variadic`.
 */
static enum compile_res compile_fn_params(struct compiler_ctx *ctx,
                                          struct lisp_val raw_params) {
  assert(ctx->current_stack_size == 0);

  // TODO Process in reverse order to avoid copying values
  // TODO Leave arguments on the stack and do some sort of upvalue management
  while (lisp_val_is_cons(raw_params)) {
    struct lisp_cons *cons_cell = lisp_val_as_obj(raw_params);
    struct lisp_val param_name = cons_cell->car;

    struct lisp_symbol *sym_name =
        lisp_val_cast(lisp_val_is_symbol, param_name);
    if (sym_name == NULL) {
      vm_raise_format_exception(ctx->vm,
                                "fn: parameter names must be of type symbol");
      return COMP_FAILED;
    }

    // compiler_add_local checks for duplicates, so no need to here
    if (compiler_add_param(ctx, sym_name) == COMP_FAILED) {
      return COMP_FAILED;
    }

    raw_params = cons_cell->cdr;
  }

  ctx->chunk->req_arg_count = ctx->current_stack_size;

  if (!lisp_val_is_nil(raw_params)) {
    ctx->chunk->is_variadic = true;

    struct lisp_symbol *rest_param =
        lisp_val_cast(lisp_val_is_symbol, raw_params);
    if (rest_param == NULL) {
      vm_raise_format_exception(ctx->vm,
                                "fn: parameter names must be of type symbol");
      return COMP_FAILED;
    }

    emit_instr(ctx, OP_BUILD_REST_ARGS);
    emit_byte(ctx, ctx->current_stack_size);

    if (compiler_add_param(ctx, rest_param) == COMP_FAILED) {
      return COMP_FAILED;
    }
  } else {
    ctx->chunk->is_variadic = false;
  }

  return COMP_SUCCESS;
}

static enum compile_res compile_fn(struct compiler_ctx *ctx,
                                   struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "fn: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val params = args_cons->car;

  args_cons = lisp_val_cast(lisp_val_is_cons, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "fn: not enough args");
    return COMP_FAILED;
  }
  struct lisp_val func_ast = args_cons->car;

  if (!lisp_val_is_nil(args_cons->cdr)) {
    vm_raise_format_exception(ctx->vm, "fn: too many args");
    return COMP_FAILED;
  }

  struct compiler_ctx *func_ctx = compiler_ctx_create_inner(ctx);
  if (ctx->binding_name != NULL) {
    func_ctx->chunk->name = ctx->binding_name;
    ctx->binding_name = NULL;
  }

  enum compile_res res = compile_fn_params(func_ctx, params);
  if (res == COMP_FAILED) {
    // Need the goto to cleanup the compiler
    goto DONE;
  }

  res = compile(func_ctx, func_ast);
  if (res == COMP_FAILED) {
    goto DONE;
  }

  // Back in the outer compiler context
  res = emit_alloc_closure(ctx, func_ctx->chunk, func_ctx->capture_count);
  if (res == COMP_FAILED) {
    goto DONE;
  }

  // Load in captured values from the current context
  for (unsigned i = 0; i < func_ctx->capture_count; i++) {
    struct capture_binding *b = &func_ctx->captures[i];
    if (b->is_local) {
      // Capture local from outer context
      emit_get_raw_local(ctx, b->index);
    } else {
      // Capture capture from outer context
      emit_get_raw_upvalue(ctx, b->index);
    }
  }

  emit_init_closure(ctx, func_ctx->capture_count);
  emit_return_if_tail_pos(ctx);

DONE:
  compiler_ctx_complete(func_ctx);
  return res;
}

static enum compile_res compile_let_bindings(struct compiler_ctx *ctx,
                                             struct lisp_val bindings) {
  uint8_t starting_local_index = ctx->local_count;
  int starting_stack_index = ctx->current_stack_size;
  int bind_count = 0;

  struct lisp_cons *bindings_cons;
  struct lisp_val rest_bindings = bindings;
  while ((bindings_cons = lisp_val_cast(lisp_val_is_cons, rest_bindings)) !=
         NULL) {
    struct lisp_cons *bind_cons =
        lisp_val_cast(lisp_val_is_cons, bindings_cons->car);
    if (bind_cons == NULL) {
      vm_raise_format_exception(
          ctx->vm, "let: each binding must be a proper list of 2 elements");
      return COMP_FAILED;
    }
    struct lisp_symbol *bind_sym =
        lisp_val_cast(lisp_val_is_symbol, bind_cons->car);
    if (bind_sym == NULL) {
      vm_raise_format_exception(
          ctx->vm, "let: first element of each binding must be a symbol");
      return COMP_FAILED;
    }

    bind_cons = lisp_val_cast(lisp_val_is_cons, bind_cons->cdr);
    if (bind_cons == NULL) {
      vm_raise_format_exception(
          ctx->vm, "let: each binding must be a proper list of 2 elements");
      return COMP_FAILED;
    }
    struct lisp_val bind_expr = bind_cons->car;

    if (!lisp_val_is_nil(bind_cons->cdr)) {
      vm_raise_format_exception(
          ctx->vm, "let: each binding must be a proper list of 2 elements");
      return COMP_FAILED;
    }

    ctx->binding_name = bind_sym;
    enum compile_res res = compile_non_tail(ctx, bind_expr);
    ctx->binding_name = NULL;
    if (res == COMP_FAILED) {
      return res;
    }

    rest_bindings = bindings_cons->cdr;
    bind_count += 1;
  }

  if (!lisp_val_is_nil(rest_bindings)) {
    vm_raise_format_exception(ctx->vm, "let: bindings must be a proper list");
    return COMP_FAILED;
  }

  // Loop through again to create the bindings
  rest_bindings = bindings;
  for (int i = 0; i < bind_count; i++) {
    // Type checks already happened above
    bindings_cons = lisp_val_cast(lisp_val_is_cons, rest_bindings);
    assert(bindings_cons != NULL);
    struct lisp_cons *bind_cons =
        lisp_val_cast(lisp_val_is_cons, bindings_cons->car);
    assert(bind_cons != NULL);
    struct lisp_symbol *bind_sym =
        lisp_val_cast(lisp_val_is_symbol, bind_cons->car);
    assert(bind_sym != NULL);

    if (compiler_lookup_local_after(ctx, bind_sym, starting_local_index) !=
        NULL) {
      vm_raise_format_exception(ctx->vm, "duplicate local binding: %s",
                                lisp_symbol_name(bind_sym));
      return COMP_FAILED;
    }

    if (compiler_add_local(ctx, bind_sym, starting_stack_index + i) ==
        COMP_FAILED) {
      return COMP_FAILED;
    }

    rest_bindings = bindings_cons->cdr;
  }

  return COMP_SUCCESS;
}

static enum compile_res compile_let(struct compiler_ctx *ctx,
                                    struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "let: not enough args");
    return COMP_FAILED;
  }

  struct lisp_val bindings = args_cons->car;
  args_cons = lisp_val_cast(lisp_val_is_cons, args_cons->cdr);
  if (args_cons == NULL) {
    vm_raise_format_exception(ctx->vm, "let: not enough args");
    return COMP_FAILED;
  }

  struct lisp_val ast = args_cons->car;
  if (!lisp_val_is_nil(args_cons->cdr)) {
    vm_raise_format_exception(ctx->vm, "let: too many args");
    return COMP_FAILED;
  }

  // Save stack state to restore it (i.e. clear the locals) after the let
  uint8_t outer_local_count = ctx->local_count;

  if (compile_let_bindings(ctx, bindings) == COMP_FAILED) {
    return COMP_FAILED;
  }

  // Allow `def!` inside `let`
  ctx->begin_pos = true;

  if (ctx->tail_pos) {
    return compile_non_top_level(ctx, ast);
  } else {
    if (compile_non_top_level(ctx, ast) == COMP_FAILED) {
      return COMP_FAILED;
    }

    int to_remove = ctx->local_count - outer_local_count;
    if (emit_skip_delete(ctx, 1, to_remove) == COMP_FAILED) {
      return COMP_FAILED;
    }
    ctx->local_count = outer_local_count;

    return COMP_SUCCESS;
  }
}

static enum compile_res compile_quote(struct compiler_ctx *ctx,
                                      struct lisp_val args) {
  struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
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
  if (ctx->tail_pos) {
    return emit_tail_call(ctx, arg_count);
  } else {
    return emit_call(ctx, arg_count);
  }
}

static enum compile_res compile_call_or_special(struct compiler_ctx *ctx,
                                                struct lisp_cons *call_ast) {
  struct lisp_val head = call_ast->car;
  struct lisp_val args = call_ast->cdr;

  if (!lisp_val_is_list(args)) {
    vm_raise_format_exception(ctx->vm, "cannot compile improper list");
    return COMP_FAILED;
  }

  struct lisp_symbol *head_sym = lisp_val_cast(lisp_val_is_symbol, head);
  if (head_sym != NULL) {
    struct resolved_sym head_resolved;
    if (resolve_symbol(ctx, head_sym, &head_resolved) == COMP_FAILED) {
      return COMP_FAILED;
    }

    switch (head_resolved.type) {
      case SYM_MACRO:
        return expand_and_compile_macro(ctx, head_resolved.value.macro_fn,
                                        args);
      case SYM_UNBOUND:
        // Check special forms
        if (head_sym == SYMBOL_FN) {
          // Keep binding name
          compile_prepare_non_def(ctx);
          return compile_fn(ctx, args);
        }
        if (head_sym == SYMBOL_LET) {
          ctx->binding_name = NULL;
          compile_prepare_non_def(ctx);
          return compile_let(ctx, args);
        }
        if (head_sym == SYMBOL_DO) {
          ctx->binding_name = NULL;
          return compile_do(ctx, args);
        }
        if (head_sym == SYMBOL_IF) {
          ctx->binding_name = NULL;
          compile_prepare_non_def(ctx);
          return compile_if(ctx, args);
        }
        if (head_sym == SYMBOL_DEF) {
          ctx->binding_name = NULL;
          return compile_def(ctx, args);
        }
        if (head_sym == SYMBOL_DEFSYNTAX) {
          ctx->binding_name = NULL;
          return compile_defsyntax(ctx, args);
        }
        if (head_sym == SYMBOL_QUOTE) {
          ctx->binding_name = NULL;
          compile_prepare_non_def(ctx);
          return compile_quote(ctx, args);
        }
        break;
      default:
        // Continue to simple call
        // TODO Use the resolved symbol to avoid re-resolving in
        // compile_func_call
        break;
    }
  }

  ctx->binding_name = NULL;
  // Not a macro or special form, so it's a normal function application
  compile_prepare_non_def(ctx);
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
  if (lisp_val_is_cons(ast)) {
    res = compile_call_or_special(ctx, lisp_val_as_obj(ast));
  } else if (lisp_val_is_symbol(ast)) {
    compile_prepare_non_def(ctx);
    res = compile_symbol_ref(ctx, lisp_val_as_obj(ast));
  } else {
    compile_prepare_non_def(ctx);
    res = compile_constant(ctx, ast);
    emit_return_if_tail_pos(ctx);
  }

  gc_pop_root_expect(ast);
  return res;
}

struct lisp_closure *compile_top_level(struct lisp_vm *vm,
                                       struct lisp_val ast) {
  struct compiler_ctx *ctx = compiler_ctx_create_top_level(vm);

  enum compile_res res = compile(ctx, ast);
  struct code_chunk *func = compiler_ctx_complete(ctx);
  if (res == COMP_FAILED) {
    return NULL;
  }

  return compile_chunk_to_closure(func);
}

enum eval_status compile_eval(struct lisp_vm *vm, struct lisp_val ast) {
  struct lisp_closure *code = compile_top_level(vm, ast);
  if (code == NULL) {
    return EV_EXCEPTION;
  }

  return eval_closure(vm, code);
}
