#include "eval.h"

#include "memory.h"
#include "types.h"
#include "vm.h"

// Permenantly allocate symbols for special forms

// TODO Include nil, true, and false in this list? (how to access in other parts
// of the program?)

static const struct lisp_symbol *SYMBOL_DO;
static const struct lisp_symbol *SYMBOL_IF;
static const struct lisp_symbol *SYMBOL_DEF;
static const struct lisp_symbol *SYMBOL_DEFMACRO;
static const struct lisp_symbol *SYMBOL_FN;
static const struct lisp_symbol *SYMBOL_QUOTE;
// static const struct lisp_symbol *SYMBOL_MACROEXPAND_1;
// static const struct lisp_symbol *SYMBOL_MACROEXPAND;
// static const struct lisp_symbol *SYMBOL_MACROEXPAND_REC;

void init_global_eval_state(void) {
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
  // DEF_GLOBAL_SYM(SYMBOL_MACROEXPAND_1, "macroexpand-1");
  // DEF_GLOBAL_SYM(SYMBOL_MACROEXPAND, "macroexpand");
  // DEF_GLOBAL_SYM(SYMBOL_MACROEXPAND_REC, "macroexpand-rec");

#undef DEF_GLOBAL_SYM
}

static enum eval_status eval_tail(struct lisp_vm *vm, struct lisp_val ast);

static struct eval_result eval_if(struct lisp_vm *vm, struct lisp_val args) {
  struct eval_result res;
  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing condition");
    res.status = EV_EXCEPTION;
    return res;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  struct lisp_val cond = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing true branch");
    res.status = EV_EXCEPTION;
    return res;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val true_branch = cell->car;
  args = cell->cdr;

  struct lisp_val false_branch;
  if (lisp_val_type(args) == LISP_CONS) {
    cell = lisp_val_as_obj(args);
    false_branch = cell->car;
    args = cell->cdr;
  } else {
    false_branch = LISP_VAL_NIL;
  }

  if (lisp_val_type(args) != LISP_NIL) {
    vm_raise_func_exception(vm, "too many args");
    res.status = EV_EXCEPTION;
    return res;
  }

  res.status = eval(vm, cond);
  if (res.status == EV_EXCEPTION) {
    return res;
  }
  struct lisp_val cond_result = vm_stack_pop(vm);

  struct lisp_val chosen_branch;
  if (lisp_val_is_false(cond_result)) {
    chosen_branch = false_branch;
  } else {
    chosen_branch = true_branch;
  }

  res.status = EV_TAIL_CALL;
  res.ast = chosen_branch;
  return res;
}

static struct eval_result eval_do(struct lisp_vm *vm, struct lisp_val args) {
  struct eval_result res;
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    vm_stack_push(vm, LISP_VAL_NIL);
    res.status = EV_SUCCESS;
    return res;
  }

  // Loop through all but the last as the last element needs to be TCO'd
  while (lisp_val_type(args_cons->cdr) == LISP_CONS) {
    // Write to result each time, but only the final itertion is kept
    res.status = eval(vm, args_cons->car);
    if (res.status == EV_EXCEPTION) {
      return res;
    }
    // Ignore the value produced by eval
    (void)vm_stack_pop(vm);

    args = args_cons->cdr;
    // Type is already checked by the while loop
    args_cons = lisp_val_as_obj(args);
  }

  // Tail call the last element
  res.status = EV_TAIL_CALL;
  res.ast = args_cons->car;
  return res;
}

static bool is_fn_def(struct lisp_val ast, struct lisp_env *env) {
  struct lisp_cons *ast_cons = lisp_val_cast(LISP_CONS, ast);
  if (ast_cons == NULL) {
    return false;
  }
  struct lisp_symbol *head_sym = lisp_val_cast(LISP_SYMBOL, ast_cons->car);
  if (head_sym == NULL) {
    return false;
  }

  // Make sure there isn't a local shadowing the definition
  const struct lisp_env_binding *binding = lisp_env_get(env, head_sym);
  if (binding != NULL) {
    return false;
  }

  return head_sym == SYMBOL_FN;
}

/**
 * Parse a def-like structure, extracting the name and evaluating the
 * expression. The evaluated value is pushed onto the stack.
 */
static enum eval_status parse_def(struct lisp_vm *vm, struct lisp_val args,
                                  struct lisp_symbol **bind_name) {
  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing symbol");
    return EV_EXCEPTION;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  if (lisp_val_type(cell->car) != LISP_SYMBOL) {
    vm_raise_func_exception(vm, "first arg must be of type symbol");
    return EV_EXCEPTION;
  }

  *bind_name = lisp_val_as_obj(cell->car);
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing value");
    return EV_EXCEPTION;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val value_expr = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    vm_raise_func_exception(vm, "too many args");
    return EV_EXCEPTION;
  }

  enum eval_status res = eval(vm, value_expr);
  if (res != EV_SUCCESS) {
    return res;
  }

  // Special handling for functions so that they can know their names
  // This doesn't work in all cases, but does for basic patterns
  if (is_fn_def(value_expr, vm_current_env(vm))) {
    struct lisp_closure *closure =
        lisp_val_cast(LISP_CLOSURE, vm_stack_top(vm));
    // The check went wrong in this case
    assert(closure != NULL);
    lisp_closure_set_name(closure, *bind_name);
  }
  return res;
}

static enum eval_status eval_def(struct lisp_vm *vm, struct lisp_val args) {
  struct lisp_symbol *bind_name;
  enum eval_status res = parse_def(vm, args, &bind_name);
  if (res != EV_SUCCESS) {
    return res;
  }

  lisp_env_set(vm_current_env(vm), bind_name, vm_stack_top(vm));
  return EV_SUCCESS;
}

static enum eval_status eval_defmacro(struct lisp_vm *vm,
                                      struct lisp_val args) {
  struct lisp_symbol *bind_name;
  enum eval_status res = parse_def(vm, args, &bind_name);
  if (res != EV_SUCCESS) {
    return res;
  }

  struct lisp_val bind_value = vm_stack_top(vm);
  if (!lisp_val_is_func(bind_value)) {
    vm_raise_func_exception(vm, "value must be a function");
    return EV_EXCEPTION;
  }

  lisp_env_set_macro(vm_current_env(vm), bind_name, bind_value);
  return EV_SUCCESS;
}

static bool param_list_contains(struct lisp_val param_list,
                                const struct lisp_symbol *param) {
  struct lisp_cons *cons;
  while ((cons = lisp_val_cast(LISP_CONS, param_list)) != NULL) {
    struct lisp_symbol *other = lisp_val_cast(LISP_SYMBOL, cons->car);
    if (other != NULL && lisp_symbol_eq(param, other)) {
      return true;
    }
    param_list = cons->cdr;
  }
  return false;
}

/**
 * Validate and process function parameters:
 * - () -> nil, nil
 * - (a b c) -> (a b c), nil
 * - (& rest) -> nil, rest
 * - (a & rest) -> (a), rest
 */
static enum eval_status process_fn_params(struct lisp_vm *vm,
                                          struct lisp_val raw_params,
                                          struct lisp_val *normal_params,
                                          struct lisp_symbol **rest_param) {
  // TODO Avoid building a new list if parameters are all "normal"
  // TODO Store parameter names in a better data structure
  struct list_builder params_builder;
  list_builder_init(&params_builder);

  // Assume none until it found
  *rest_param = NULL;

  enum eval_status res = EV_SUCCESS;
  while (lisp_val_type(raw_params) == LISP_CONS) {
    struct lisp_cons *cons_cell = lisp_val_as_obj(raw_params);
    struct lisp_val param_name = cons_cell->car;
    raw_params = cons_cell->cdr;

    struct lisp_symbol *sym_name = lisp_val_cast(LISP_SYMBOL, param_name);
    if (sym_name == NULL) {
      vm_raise_func_exception(vm, "parameter names must be of type symbol");
      res = EV_EXCEPTION;
      break;
    }

    if (param_list_contains(list_builder_current(&params_builder), sym_name)) {
      vm_raise_func_exception(vm, "duplicate parameter: %s",
                              lisp_symbol_name(sym_name));
      res = EV_EXCEPTION;
      break;
    }

    list_builder_append(&params_builder, param_name);
  }

  if (res == EV_SUCCESS && !lisp_val_is_nil(raw_params)) {
    *rest_param = lisp_val_cast(LISP_SYMBOL, raw_params);
    if (*rest_param == NULL) {
      vm_raise_func_exception(vm, "parameter names must be of type symbol");
      res = EV_EXCEPTION;
    } else if (param_list_contains(list_builder_current(&params_builder),
                                   *rest_param)) {
      vm_raise_func_exception(vm, "duplicate parameter: %s",
                              lisp_symbol_name(*rest_param));
      res = EV_EXCEPTION;
    }
  }

  // Build regardless of success
  *normal_params = list_build(&params_builder);
  return res;
}

static enum eval_status eval_fn(struct lisp_vm *vm, struct lisp_val args) {
  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing binding list");
    return EV_EXCEPTION;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  struct lisp_val raw_params = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, "missing expression");
    return EV_EXCEPTION;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val expr = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    vm_raise_func_exception(vm, "too many args");
    return EV_EXCEPTION;
  }

  struct lisp_val fn_params;
  struct lisp_symbol *fn_rest_param;
  enum eval_status res =
      process_fn_params(vm, raw_params, &fn_params, &fn_rest_param);
  if (res == EV_EXCEPTION) {
    return res;
  }

  vm_stack_push(vm, lisp_val_from_obj(lisp_closure_create(
                        fn_params, fn_rest_param, vm_current_env(vm), expr)));
  return EV_SUCCESS;
}

static enum eval_status eval_quote(struct lisp_vm *vm, struct lisp_val args) {
  struct lisp_cons *cell = lisp_val_cast(LISP_CONS, args);
  if (cell == NULL) {
    vm_raise_func_exception(vm, "missing argument");
    return EV_EXCEPTION;
  }
  struct lisp_val quoted = cell->car;

  if (!lisp_val_is_nil(cell->cdr)) {
    vm_raise_func_exception(vm, "too many args");
    return EV_EXCEPTION;
  }

  vm_stack_push(vm, quoted);
  return EV_SUCCESS;
}

static struct eval_result call_closure(struct lisp_vm *vm,
                                       struct lisp_closure *cl,
                                       unsigned stack_arg_count) {
  // Save the closure while doing the environment operations since the closure
  // could be an immediate value
  gc_push_root_obj(cl);

  // Create new environment
  struct lisp_env *func_env = lisp_env_create(lisp_closure_env(cl));
  vm_create_tail_stack_frame(vm, func_env, stack_arg_count);

  // Bind arguments to environment
  struct eval_result res;

  struct lisp_val params = lisp_closure_params(cl);
  struct lisp_cons *params_cons;
  unsigned param_index = 0;
  while ((params_cons = lisp_val_cast(LISP_CONS, params)) != NULL) {
    lisp_env_set(func_env, lisp_val_cast(LISP_SYMBOL, params_cons->car),
                 vm_from_frame_pointer(vm, param_index));

    params = params_cons->cdr;
    param_index++;
  }

  struct lisp_symbol *rest_param = lisp_closure_rest_param(cl);
  if (rest_param != NULL) {
    lisp_env_set(func_env, rest_param, vm_from_stack_pointer(vm, 0));
  }

  // Don't need the stack items anymore now that they are bound
  vm_stack_frame_clear(vm);

  gc_pop_root_expect_obj(cl);

  res.status = EV_TAIL_CALL;
  res.ast = lisp_closure_ast(cl);
  return res;
}

static bool validate_function(struct lisp_val func, unsigned *arg_count,
                              bool *is_variadic) {
  enum lisp_type func_type = lisp_val_type(func);
  if (func_type == LISP_BUILTIN) {
    struct lisp_builtin *builtin = lisp_val_as_obj(func);
    *arg_count = builtin->arg_count;
    *is_variadic = builtin->has_rest_arg;
    return true;
  } else if (func_type == LISP_CLOSURE) {
    struct lisp_closure *closure = lisp_val_as_obj(func);
    *arg_count = lisp_closure_arg_count(closure);
    *is_variadic = lisp_closure_rest_param(closure) != NULL;
    return true;
  } else {
    return false;
  }
}

static struct eval_result dispatch_call(struct lisp_vm *vm,
                                        struct lisp_val func,
                                        unsigned stack_arg_count) {
  enum lisp_type func_type = lisp_val_type(func);
  struct eval_result res;
  if (func_type == LISP_BUILTIN) {
    struct lisp_builtin *builtin = lisp_val_as_obj(func);
    vm_create_tail_stack_frame(vm, vm_current_env(vm), stack_arg_count);
    // TODO Enable tail calls from builtins
    res.status = builtin->func(vm);
    return res;
  } else if (func_type == LISP_CLOSURE) {
    return call_closure(vm, lisp_val_as_obj(func), stack_arg_count);
  } else {
    vm_raise_func_exception(vm, "cannot call value of type: %s",
                            lisp_val_type_name(func));
    // Should have already been checked
    res.status = EV_EXCEPTION;
  }
  return res;
}

static enum eval_status eval_and_build_rest_args(struct lisp_vm *vm,
                                                 struct lisp_val arg_exprs,
                                                 struct lisp_val *arg_values) {
  // TODO Use the VM stack to save the in-progess list instead of GC roots?
  struct list_builder builder;
  list_builder_init(&builder);
  enum eval_status res = EV_SUCCESS;

  struct lisp_cons *args_cons;
  while ((args_cons = lisp_val_cast(LISP_CONS, arg_exprs)) != NULL) {
    res = eval(vm, args_cons->car);
    if (res == EV_EXCEPTION) {
      break;
    }
    list_builder_append(&builder, vm_stack_pop(vm));

    arg_exprs = args_cons->cdr;
  }

  *arg_values = list_build(&builder);
  return res;
}

/**
 * Call a function structure. The function and all arguments are first
 * evaluated, then pushed on the stack.
 */
static struct eval_result eval_call(struct lisp_vm *vm,
                                    struct lisp_val func_expr,
                                    struct lisp_val args_expr) {
  struct eval_result res;

  // Evaluate function first to make sure it's callable and find the number of
  // args
  res.status = eval(vm, func_expr);
  if (res.status == EV_EXCEPTION) {
    return res;
  }
  struct lisp_val func = vm_stack_pop(vm);

  unsigned arg_count;
  bool is_variadic;
  if (!validate_function(func, &arg_count, &is_variadic)) {
    vm_raise_func_exception(vm, "cannot call object of type: %s",
                            lisp_val_type_name(func));
    res.status = EV_EXCEPTION;
    return res;
  }

  gc_push_root(func);

  for (unsigned i = 0; i < arg_count; i++) {
    struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args_expr);
    if (args_cons == NULL) {
      vm_raise_func_exception(vm, "not enough arguments");
      res.status = EV_EXCEPTION;
      goto ERROR;
    }

    res.status = eval(vm, args_cons->car);
    if (res.status == EV_EXCEPTION) {
      goto ERROR;
    }

    args_expr = args_cons->cdr;
  }

  if (is_variadic) {
    struct lisp_val rest_args_values;
    res.status = eval_and_build_rest_args(vm, args_expr, &rest_args_values);
    if (res.status == EV_EXCEPTION) {
      goto ERROR;
    }
    vm_stack_push(vm, rest_args_values);
  } else if (!lisp_val_is_nil(args_expr)) {
    vm_raise_func_exception(vm, "too many arguments");
    res.status = EV_EXCEPTION;
    goto ERROR;
  }

  gc_pop_root_expect(func);
  // Total number of stack elements passed to the function
  unsigned stack_arg_count = arg_count + (is_variadic ? 1 : 0);
  return dispatch_call(vm, func, stack_arg_count);

ERROR:
  gc_pop_root_expect(func);
  return res;
}

/**
 * Apply a function to a list of arguments. Neither the function nor arguments
 * are evaluated.
 */
static struct eval_result eval_apply_tco(struct lisp_vm *vm,
                                         struct lisp_val func,
                                         struct lisp_val args) {
  struct eval_result res;

  unsigned arg_count;
  bool is_variadic;
  if (!validate_function(func, &arg_count, &is_variadic)) {
    vm_raise_func_exception(vm, "cannot call object of type: %s",
                            lisp_val_type_name(func));
    res.status = EV_EXCEPTION;
    return res;
  }

  for (unsigned i = 0; i < arg_count; i++) {
    struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
    if (args_cons == NULL) {
      vm_raise_func_exception(vm, "not enough arguments");
      res.status = EV_EXCEPTION;
      return res;
    }

    vm_stack_push(vm, args_cons->car);
    args = args_cons->cdr;
  }

  if (is_variadic) {
    // Args are not evaluated, so the rest of the list can be used directly
    vm_stack_push(vm, args);
  } else if (!lisp_val_is_nil(args)) {
    vm_raise_func_exception(vm, "too many arguments");
    res.status = EV_EXCEPTION;
    return res;
  }

  // Total number of stack elements passed to the function
  unsigned stack_arg_count = arg_count + (is_variadic ? 1 : 0);
  return dispatch_call(vm, func, stack_arg_count);
}

enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func,
                            struct lisp_val args) {
  vm_create_stack_frame(vm, vm_current_env(vm), 0);
  struct eval_result res = eval_apply_tco(vm, func, args);
  switch (res.status) {
    case EV_SUCCESS:
      vm_stack_frame_return(vm);
      return EV_SUCCESS;
    case EV_TAIL_CALL:
      res.status = eval_tail(vm, res.ast);
      return res.status;
    case EV_EXCEPTION:
      return EV_EXCEPTION;
    default:
      return EV_EXCEPTION;
  }
}

static struct eval_result do_macro_expand(struct lisp_vm *vm,
                                          struct lisp_val macro_fn,
                                          struct lisp_val args) {
  struct eval_result res;
  // TODO Extracted portion of eval_apply which doesn't need to do type
  // checking?
  res.status = eval_apply(vm, macro_fn, args);
  if (res.status == EV_EXCEPTION) {
    return res;
  }

  res.status = EV_TAIL_CALL;
  res.ast = vm_stack_pop(vm);
  return res;
}

static struct eval_result eval_special_or_call(struct lisp_vm *vm,
                                               struct lisp_val ast) {
  struct lisp_cons *call_ast = lisp_val_as_obj(ast);
  struct lisp_val head = call_ast->car;
  struct lisp_val args = call_ast->cdr;

  // Check the structure here to avoid doing it everywhere else
  if (!lisp_val_is_list(args)) {
    vm_raise_func_exception(vm, "cannot evaluate improper list");
    return (struct eval_result){.status = EV_EXCEPTION};
  }

  // Then special forms
  if (lisp_val_type(head) == LISP_SYMBOL) {
    struct lisp_symbol *head_sym = lisp_val_as_obj(head);

    const struct lisp_env_binding *binding =
        lisp_env_get(vm_current_env(vm), head_sym);
    if (binding != NULL) {
      if (binding->is_macro) {
        // Doesn't need result since it always returns a tail call
        return do_macro_expand(vm, binding->value, args);
      } else {
        // Must be a function
        // TODO Avoid looking up the symbol again
        return eval_call(vm, head, args);
      }
    } else {
      struct eval_result res;

      // Check special forms
      if (lisp_symbol_eq(head_sym, SYMBOL_DO)) {
        return eval_do(vm, args);
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_IF)) {
        return eval_if(vm, args);
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_DEF)) {
        res.status = eval_def(vm, args);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_DEFMACRO)) {
        res.status = eval_defmacro(vm, args);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_FN)) {
        res.status = eval_fn(vm, args);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_QUOTE)) {
        res.status = eval_quote(vm, args);
        return res;
      }
      // Non-matching case handled by eval_call below

      // TODO These actually need to be functions if they are to act as they do
      // in scheme/clojure. I.e. they take in a quoted AST
      // TODO The semantics of these are broken anyway
      /*
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND_1)) {
        res.status = eval_macro_expand_once(vm, args);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND)) {
        res.status = eval_macro_expand_full(vm, args);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND_REC)) {
        res.status = eval_macro_expand_recursive(vm, args);
        return res;
      }
      */

      // Implemented in prelude
      // if (lisp_symbol_eq(head_sym, SYMBOL_QUASIQUOTE)) {
      //   res.status = eval_quasiquote(args, result);
      //   return res;
      // }
    }
  }

  // Fallback if no special form matched
  return eval_call(vm, head, args);
}

static enum eval_status eval_tail(struct lisp_vm *vm, struct lisp_val ast) {
  while (true) {
    gc_push_root(ast);

    // struct lisp_string *printed = print_str(ast, true);
    // log("%s", lisp_string_as_cstr(printed));

    struct eval_result res;
    switch (lisp_val_type(ast)) {
      case LISP_NIL:
      case LISP_INT:
      case LISP_REAL:
      case LISP_CHAR:
      case LISP_STRING:
      case LISP_BUILTIN:
      case LISP_CLOSURE:
      case LISP_ATOM:
        vm_stack_push(vm, ast);
        res.status = EV_SUCCESS;
        break;
      case LISP_SYMBOL: {
        struct lisp_symbol *sym = lisp_val_as_obj(ast);
        const struct lisp_env_binding *found =
            lisp_env_get(vm_current_env(vm), sym);
        if (found != NULL) {
          vm_stack_push(vm, found->value);
          res.status = EV_SUCCESS;
        } else {
          vm_raise_func_exception(vm, "symbol '%s' not defined",
                                  lisp_symbol_name(sym));
          res.status = EV_EXCEPTION;
        }
        break;
      }
      case LISP_CONS: {
        res = eval_special_or_call(vm, ast);
        break;
      }
      default:
        vm_raise_func_exception(vm, "cannot evaluate object of type: %s",
                                lisp_val_type_name(ast));
        res.status = EV_EXCEPTION;
        break;
    }

    gc_pop_root_expect(ast);

    switch (res.status) {
      case EV_SUCCESS:
        vm_stack_frame_return(vm);
        return res.status;
      case EV_TAIL_CALL:
        ast = res.ast;
        continue;
      case EV_EXCEPTION:
        return res.status;
    }
  }
}

enum eval_status eval(struct lisp_vm *vm, struct lisp_val ast) {
  vm_create_stack_frame(vm, vm_current_env(vm), 0);

  eval_tail(vm, ast);
}
