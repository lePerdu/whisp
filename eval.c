#include "eval.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "log.h"
#include "memory.h"
#include "printer.h"
#include "types.h"

struct lisp_env *global_env = NULL;

struct lisp_val current_exception;

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

void init_global_env(void) {
  global_env = lisp_env_create(NULL);
  gc_push_root_obj(global_env);

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

void set_format_exception(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  current_exception = lisp_val_from_obj(lisp_string_vformat(format, ap));
  va_end(ap);
}

static enum eval_status eval_cons(struct lisp_cons *ast_list,
                                  struct lisp_env *env,
                                  struct lisp_cons **result,
                                  bool *result_is_list) {
  struct list_builder builder;
  list_builder_init(&builder);
  enum eval_status res = EV_SUCCESS;

  struct lisp_val ast = lisp_val_from_obj(ast_list);

  enum lisp_type ast_type;
  // Check type at the end since we know it's a list to start off
  do {
    ast_list = lisp_val_as_obj(ast);

    struct lisp_val next;
    res = eval(ast_list->car, env, &next);
    if (res == EV_EXCEPTION) {
      goto ERROR;
    }
    list_builder_append(&builder, next);

    ast = ast_list->cdr;
  } while ((ast_type = lisp_val_type(ast)) == LISP_CONS);

  if (ast_type != LISP_NIL) {
    *result_is_list = false;
    struct lisp_val end;
    res = eval(ast, env, &end);
    if (res == EV_EXCEPTION) {
      goto ERROR;
    }
    list_builder_end_pair(&builder, end);
  } else {
    *result_is_list = true;
  }

  *result = lisp_val_as_obj(list_build(&builder));
  return res;

ERROR:
  // Don't assign to result as it may not be the correct type
  list_build(&builder);
  return res;
}

static struct eval_result eval_call_closure(struct lisp_closure *cl,
                                            struct lisp_val args,
                                            struct lisp_val *result) {
  // Make sure these don't go away while building up the environment (which can
  // allocate)
  gc_push_root(lisp_val_from_obj(cl));
  gc_push_root(args);

  struct lisp_env *fn_env = lisp_env_create(lisp_closure_env(cl));
  gc_push_root(lisp_val_from_obj(fn_env));

  struct eval_result res = {.status = EV_SUCCESS};

  struct lisp_val param_names = lisp_closure_params(cl);
  while (lisp_val_type(param_names) == LISP_CONS) {
    struct lisp_cons *params_cons = lisp_val_as_obj(param_names);
    // Assume parameters correctly set
    struct lisp_symbol *param_name = lisp_val_as_obj(params_cons->car);
    param_names = params_cons->cdr;

    if (lisp_val_type(args) != LISP_CONS) {
      set_func_exception("not enough arguments");
      res.status = EV_EXCEPTION;
      goto END;
    }
    struct lisp_cons *args_cons = lisp_val_as_obj(args);
    struct lisp_val param_value = args_cons->car;
    args = args_cons->cdr;

    lisp_env_set(fn_env, param_name, param_value);
  }

  struct lisp_symbol *rest_param = lisp_closure_rest_param(cl);
  if (rest_param != NULL) {
    lisp_env_set(fn_env, rest_param, args);
  } else if (lisp_val_type(args) != LISP_NIL) {
    set_func_exception("too many arguments");
    res.status = EV_EXCEPTION;
    goto END;
  }

END:
  gc_pop_root_expect(lisp_val_from_obj(fn_env));
  gc_pop_root();  // args variable is mutated
  gc_pop_root_expect(lisp_val_from_obj(cl));

  if (res.status != EV_EXCEPTION) {
    res.status = EV_TAIL_CALL;
    res.ast = lisp_closure_ast(cl);
    res.env = fn_env;
    (void)result;
  }
  return res;
}

struct eval_result eval_apply_tco(struct lisp_val func, struct lisp_val args,
                                  struct lisp_val *result) {
  struct eval_result res;
  enum lisp_type func_type = lisp_val_type(func);
  if (func_type == LISP_BUILTIN) {
    struct lisp_builtin *builtin = lisp_val_as_obj(func);
    // Push the arguments in case the builtin allocates
    gc_push_root(args);
    res.status = builtin->func(args, result);
    gc_pop_root_expect(args);
  } else if (func_type == LISP_CLOSURE) {
    struct lisp_closure *cl = lisp_val_as_obj(func);
    res = eval_call_closure(cl, args, result);
  } else {
    set_func_exception("cannot call object of type: %s",
                       lisp_val_type_name(func));
    res.status = EV_EXCEPTION;
  }
  return res;
}

/**
 * Non-TCO-enabled version for use in builtins.
 * TODO Enable TCO from builtins?
 */
enum eval_status eval_apply(struct lisp_val func, struct lisp_val args,
                            struct lisp_val *result) {
  struct eval_result res = eval_apply_tco(func, args, result);
  if (res.status == EV_TAIL_CALL) {
    return eval(res.ast, res.env, result);
  } else {
    return res.status;
  }
}

static struct eval_result eval_call(struct lisp_cons *call_ast,
                                    struct lisp_env *env,
                                    struct lisp_val *result) {
  struct lisp_cons *evaled_list;
  bool is_list;
  struct eval_result res;
  res.status = eval_cons(call_ast, env, &evaled_list, &is_list);
  if (res.status == EV_EXCEPTION) {
    return res;
  }
  if (!is_list) {
    set_func_exception("cannot call improper list");
    res.status = EV_EXCEPTION;
    return res;
  }

  return eval_apply_tco(evaled_list->car, evaled_list->cdr, result);
}

static struct eval_result do_macro_expand(struct lisp_val macro_fn,
                                          struct lisp_val args,
                                          struct lisp_env *env) {
  struct eval_result res;
  // TODO Extracted portion of eval_apply which doesn't need to do type
  // checking?
  struct lisp_val expanded_ast;
  res.status = eval_apply(macro_fn, args, &expanded_ast);
  if (res.status == EV_EXCEPTION) {
    return res;
  }

  res.status = EV_TAIL_CALL;
  res.ast = expanded_ast;
  res.env = env;
  return res;
}

static struct eval_result eval_if(struct lisp_val args, struct lisp_env *env,
                                  struct lisp_val *result) {
  struct eval_result res;
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing condition");
    res.status = EV_EXCEPTION;
    return res;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  struct lisp_val cond = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing true branch");
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
    set_func_exception("too many args");
    res.status = EV_EXCEPTION;
    return res;
  }

  struct lisp_val cond_result;
  res.status = eval(cond, env, &cond_result);
  if (res.status == EV_EXCEPTION) {
    return res;
  }

  struct lisp_val chosen_branch;
  if (lisp_val_is_false(cond_result)) {
    chosen_branch = false_branch;
  } else {
    chosen_branch = true_branch;
  }

  res.status = EV_TAIL_CALL;
  res.ast = chosen_branch;
  res.env = env;
  (void)result;
  return res;
}

static struct eval_result eval_do(struct lisp_val args, struct lisp_env *env,
                                  struct lisp_val *result) {
  struct eval_result res;
  if (lisp_val_type(args) == LISP_NIL) {
    *result = LISP_VAL_NIL;
    res.status = EV_SUCCESS;
    return res;
  }
  if (lisp_val_type(args) != LISP_CONS) {
    res.status = EV_EXCEPTION;
    return res;
  }

  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  // Loop through all but the last as the last element needs to be TCO'd
  while (lisp_val_type(args_cons->cdr) == LISP_CONS) {
    // Write to result each time, but only the final itertion is kept
    res.status = eval(args_cons->car, env, result);
    if (res.status == EV_EXCEPTION) {
      return res;
    }
    args = args_cons->cdr;
    // Type is already checked by the while loop
    args_cons = lisp_val_as_obj(args);
  }

  // TODO Check structure before evaluating anything
  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_func_exception("cannot evaluate pair");
    res.status = EV_EXCEPTION;
    return res;
  }

  // Tail call the last element
  res.status = EV_TAIL_CALL;
  res.ast = args_cons->car;
  res.env = env;
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

static enum eval_status parse_def(struct lisp_val args, struct lisp_env *env,
                                  struct lisp_symbol **bind_name,
                                  struct lisp_val *bind_value) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing symbol");
    return EV_EXCEPTION;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  if (lisp_val_type(cell->car) != LISP_SYMBOL) {
    set_func_exception("first arg must be of type symbol");
    return EV_EXCEPTION;
  }

  *bind_name = lisp_val_as_obj(cell->car);
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing value");
    return EV_EXCEPTION;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val value_expr = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  enum eval_status res = eval(value_expr, env, bind_value);
  if (res != EV_SUCCESS) {
    return res;
  }

  // Special handling for functions so that they can know their names
  // This doesn't work in all cases, but does for basic patterns
  if (is_fn_def(value_expr, env)) {
    struct lisp_closure *closure = lisp_val_cast(LISP_CLOSURE, *bind_value);
    // The check went wrong in this case
    assert(closure != NULL);
    lisp_closure_set_name(closure, *bind_name);
  }
  return res;
}

static enum eval_status eval_def(struct lisp_val args, struct lisp_env *env,
                                 struct lisp_val *result) {
  struct lisp_symbol *bind_name;
  struct lisp_val bind_value;
  enum eval_status res = parse_def(args, env, &bind_name, &bind_value);
  if (res != EV_SUCCESS) {
    return res;
  }

  lisp_env_set(env, bind_name, bind_value);
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

static enum eval_status eval_defmacro(struct lisp_val args,
                                      struct lisp_env *env,
                                      struct lisp_val *result) {
  struct lisp_symbol *bind_name;
  struct lisp_val bind_value;
  enum eval_status res = parse_def(args, env, &bind_name, &bind_value);
  if (res != EV_SUCCESS) {
    return res;
  }

  if (!lisp_val_is_func(bind_value)) {
    set_func_exception("value must be a function");
    return EV_EXCEPTION;
  }

  lisp_env_set_macro(env, bind_name, bind_value);
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

/*
 * TODO Is undef! useful?
static enum eval_status eval_undef(struct lisp_val args, struct lisp_env *env,
                                   struct lisp_val *result) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    set_func_exception("missing symbol");
    return EV_EXCEPTION;
  }

  struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, args_cons->car);
  if (sym == NULL) {
    set_func_exception("argument must be of type symbol");
    return EV_EXCEPTION;
  }

  if (!lisp_val_is_nil(args_cons->cdr)) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  lisp_env_unset(env, sym);
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}
*/

/*
static enum eval_status process_let_binding(struct lisp_val bindings,
                                            struct lisp_env *let_env) {
  while (lisp_val_type(bindings) == LISP_CONS) {
    struct lisp_cons *bindings_head = lisp_val_as_obj(bindings);
    struct lisp_val binding = bindings_head->car;
    bindings = bindings_head->cdr;

    if (lisp_val_type(binding) != LISP_CONS) {
      set_func_exception("binding must be of type pair");
      return EV_EXCEPTION;
    }
    struct lisp_cons *bind_cons = lisp_val_as_obj(binding);

    if (lisp_val_type(bind_cons->car) != LISP_SYMBOL) {
      set_func_exception("first element of binding must be of type symbol");
      return EV_EXCEPTION;
    }
    struct lisp_symbol *bind_sym = lisp_val_as_obj(bind_cons->car);

    binding = bind_cons->cdr;
    if (lisp_val_type(binding) != LISP_CONS) {
      set_func_exception("missing binding value");
      return EV_EXCEPTION;
    }
    bind_cons = lisp_val_as_obj(binding);
    struct lisp_val bind_expr = bind_cons->car;

    if (lisp_val_type(bind_cons->cdr) != LISP_NIL) {
      set_func_exception("binding must be of type list with 2 elements");
      return EV_EXCEPTION;
    }

    struct lisp_val bind_val;
    enum eval_status res = eval(bind_expr, let_env, &bind_val);
    if (res == EV_EXCEPTION) {
      return res;
    }
    lisp_env_set(let_env, bind_sym, bind_val);
  }

  if (lisp_val_type(bindings) != LISP_NIL) {
    set_func_exception("bindings must be a list");
    return EV_EXCEPTION;
  }

  return EV_SUCCESS;
}

static struct eval_result eval_let(struct lisp_val args,
                                   struct lisp_env *outer_env,
                                   struct lisp_val *result) {
  struct eval_result res;
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing binding list");
    res.status = EV_EXCEPTION;
    return res;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  struct lisp_val bindings = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing expression");
    res.status = EV_EXCEPTION;
    return res;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val expr = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    set_func_exception("too many args");
    res.status = EV_EXCEPTION;
    return res;
  }

  struct lisp_env *let_env = lisp_env_create(outer_env);
  // Save env while building it up since the environment can re-allocate
  gc_push_root(lisp_val_from_obj(let_env));
  res.status = process_let_binding(bindings, let_env);
  gc_pop_root_expect(lisp_val_from_obj(let_env));

  if (res.status == EV_EXCEPTION) {
    return res;
  }

  res.status = EV_TAIL_CALL;
  res.ast = expr;
  res.env = let_env;
  (void)result; // Taken care of in the tail call
  return res;
}
*/

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
static enum eval_status process_fn_params(struct lisp_val raw_params,
                                          struct lisp_val *normal_params,
                                          struct lisp_symbol **rest_param) {
  // TODO Avoid building a new list if parameters are all "normal"
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
      set_func_exception("parameter names must be of type symbol");
      res = EV_EXCEPTION;
      break;
    }

    if (param_list_contains(list_builder_current(&params_builder), sym_name)) {
      set_func_exception("duplicate parameter: %s", lisp_symbol_name(sym_name));
      res = EV_EXCEPTION;
      break;
    }

    list_builder_append(&params_builder, param_name);
  }

  if (res == EV_SUCCESS && !lisp_val_is_nil(raw_params)) {
    *rest_param = lisp_val_cast(LISP_SYMBOL, raw_params);
    if (*rest_param == NULL) {
      set_func_exception("parameter names must be of type symbol");
      res = EV_EXCEPTION;
    } else if (param_list_contains(list_builder_current(&params_builder),
                                   *rest_param)) {
      set_func_exception("duplicate parameter: %s",
                         lisp_symbol_name(*rest_param));
      res = EV_EXCEPTION;
    }
  }

  // Build regardless of success
  *normal_params = list_build(&params_builder);
  return res;
}

static enum eval_status eval_fn(struct lisp_val args,
                                struct lisp_env *outer_env,
                                struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing binding list");
    return EV_EXCEPTION;
  }
  struct lisp_cons *cell = lisp_val_as_obj(args);
  struct lisp_val raw_params = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing expression");
    return EV_EXCEPTION;
  }
  cell = lisp_val_as_obj(args);
  struct lisp_val expr = cell->car;
  args = cell->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  struct lisp_val fn_params;
  struct lisp_symbol *fn_rest_param;
  enum eval_status res =
      process_fn_params(raw_params, &fn_params, &fn_rest_param);
  if (res == EV_EXCEPTION) {
    return res;
  }

  *result = lisp_val_from_obj(
      lisp_closure_create(fn_params, fn_rest_param, outer_env, expr));
  return EV_SUCCESS;
}

static enum eval_status eval_quote(struct lisp_val args,
                                   struct lisp_val *result) {
  struct lisp_cons *cell = lisp_val_cast(LISP_CONS, args);
  if (cell == NULL) {
    set_func_exception("missing argument");
    return EV_EXCEPTION;
  }
  struct lisp_val quoted = cell->car;

  if (!lisp_val_is_nil(cell->cdr)) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  *result = quoted;
  return EV_SUCCESS;
}

/*
static enum eval_status eval_macro_expand_once(struct lisp_val args,
                                               struct lisp_env *env,
                                               struct lisp_val *result) {
  struct lisp_cons *cell = lisp_val_cast(LISP_CONS, args);
  if (cell == NULL) {
    set_func_exception("missing argument");
    return EV_EXCEPTION;
  }
  struct lisp_val ast = cell->car;

  if (!lisp_val_is_nil(cell->cdr)) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  return macro_expand_once(ast, env, result);
}

static enum eval_status eval_macro_expand_full(struct lisp_val args,
                                               struct lisp_env *env,
                                               struct lisp_val *result) {
  struct lisp_cons *cell = lisp_val_cast(LISP_CONS, args);
  if (cell == NULL) {
    set_func_exception("missing argument");
    return EV_EXCEPTION;
  }
  struct lisp_val ast = cell->car;

  if (!lisp_val_is_nil(cell->cdr)) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  return macro_expand_full(ast, env, result);
}

static enum eval_status eval_macro_expand_recursive(struct lisp_val args,
                                                    struct lisp_env *env,
                                                    struct lisp_val *result) {
  struct lisp_cons *cell = lisp_val_cast(LISP_CONS, args);
  if (cell == NULL) {
    set_func_exception("missing argument");
    return EV_EXCEPTION;
  }
  struct lisp_val ast = cell->car;

  if (!lisp_val_is_nil(cell->cdr)) {
    set_func_exception("too many args");
    return EV_EXCEPTION;
  }

  return macro_expand_recursive(ast, env, result);
}
*/

static struct eval_result eval_special_or_call(struct lisp_val ast,
                                               struct lisp_env *env,
                                               struct lisp_val *result) {
  struct lisp_cons *call_ast = lisp_val_as_obj(ast);

  // Then special forms
  if (lisp_val_type(call_ast->car) == LISP_SYMBOL) {
    struct lisp_symbol *head_sym = lisp_val_as_obj(call_ast->car);
    struct lisp_val args = call_ast->cdr;

    const struct lisp_env_binding *binding = lisp_env_get(env, head_sym);
    if (binding != NULL) {
      if (binding->is_macro) {
        // Doesn't need result since it always returns a tail call
        return do_macro_expand(binding->value, args, env);
      } else {
        // Must be a function
        // TODO Avoid looking up the symbol again
        return eval_call(call_ast, env, result);
      }
    } else {
      struct eval_result res;

      // Check special forms
      if (lisp_symbol_eq(head_sym, SYMBOL_DO)) {
        return eval_do(args, env, result);
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_IF)) {
        return eval_if(args, env, result);
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_DEF)) {
        res.status = eval_def(args, env, result);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_DEFMACRO)) {
        res.status = eval_defmacro(args, env, result);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_FN)) {
        res.status = eval_fn(args, env, result);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_QUOTE)) {
        res.status = eval_quote(args, result);
        return res;
      }
      // Non-matching case handled by eval_call below

      // TODO These actually need to be functions if they are to act as they do
      // in scheme/clojure. I.e. they take in a quoted AST
      // TODO The semantics of these are broken anyway
      /*
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND_1)) {
        res.status = eval_macro_expand_once(args, env, result);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND)) {
        res.status = eval_macro_expand_full(args, env, result);
        return res;
      }
      if (lisp_symbol_eq(head_sym, SYMBOL_MACROEXPAND_REC)) {
        res.status = eval_macro_expand_recursive(args, env, result);
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
  return eval_call(call_ast, env, result);
}

enum eval_status eval(struct lisp_val ast, struct lisp_env *env,
                      struct lisp_val *result) {
  while (true) {
    gc_push_root(ast);
    gc_push_root(lisp_val_from_obj(env));

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
        *result = ast;
        res.status = EV_SUCCESS;
        break;
      case LISP_SYMBOL: {
        struct lisp_symbol *sym = lisp_val_as_obj(ast);
        const struct lisp_env_binding *found = lisp_env_get(env, sym);
        if (found != NULL) {
          *result = found->value;
          res.status = EV_SUCCESS;
        } else {
          set_func_exception("symbol '%s' not defined", lisp_symbol_name(sym));
          res.status = EV_EXCEPTION;
        }
        break;
      }
      case LISP_CONS: {
        res = eval_special_or_call(ast, env, result);
        break;
      }
      default:
        set_func_exception("cannot evaluate object of type: %s",
                           lisp_val_type_name(ast));
        res.status = EV_EXCEPTION;
        break;
    }

    gc_pop_root_expect(lisp_val_from_obj(env));
    gc_pop_root_expect(ast);

    if (res.status == EV_TAIL_CALL) {
      ast = res.ast;
      env = res.env;
      continue;
    } else {
      return res.status;
    }
  }
}
