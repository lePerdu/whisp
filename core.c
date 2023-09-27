#include "core.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <time.h>

#include "bytecode.h"
#include "compiler.h"
#include "core_helper.h"
#include "eval.h"
#include "file.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "types.h"
#include "vm.h"

MAKE_INT_BINARY(add, +);
MAKE_INT_BINARY(sub, -);
MAKE_INT_BINARY(mul, *);

DEF_BUILTIN(core_int_div) {
  DEF_INT_ARG(dividend, 0);
  DEF_INT_ARG(divisor, 1);
  if (divisor == 0) {
    vm_raise_func_exception(vm, "cannot divide by 0");
    return EV_EXCEPTION;
  }
  BUILTIN_RETURN(lisp_val_from_int(dividend / divisor));
}

MAKE_INT_BINARY(bitand, &);
MAKE_INT_BINARY(bitor, |);
MAKE_INT_BINARY(bitxor, ^);

static enum eval_status core_int_bitshift(struct lisp_vm *vm) {
  DEF_INT_ARG(x, 0);
  DEF_INT_ARG(shift, 1);

  BUILTIN_RETURN(lisp_val_from_int(
      shift >= 0 ? (unsigned long)(x << shift)
                 // Cast to unsigned to avoid sign extension
                 // TODO Figure out if this is necessary
                 : ((unsigned long)x >> (unsigned long)-shift)));
}

MAKE_INT_COMPARE(lt, <);
MAKE_INT_COMPARE(lte, <=);
MAKE_INT_COMPARE(gt, >);
MAKE_INT_COMPARE(gte, >=);
MAKE_INT_COMPARE(eq, ==);

MAKE_REAL_BINARY(add, +);
MAKE_REAL_BINARY(sub, -);
MAKE_REAL_BINARY(mul, *);
// No special handling for floating-point divide
MAKE_REAL_BINARY(div, /);

static enum eval_status do_real_exp(struct lisp_val x,
                                    struct lisp_val *result) {
  *result = lisp_val_from_real(exp(lisp_val_as_real(x)));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_real_exp) {
  return unary_func_with_type(vm, __func__, LISP_REAL, do_real_exp);
}

static enum eval_status do_real_log(struct lisp_val x,
                                    struct lisp_val *result) {
  *result = lisp_val_from_real(log(lisp_val_as_real(x)));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_real_log) {
  return unary_func_with_type(vm, __func__, LISP_REAL, do_real_log);
};

static enum eval_status do_real_pow(struct lisp_val x, struct lisp_val y,
                                    struct lisp_val *result) {
  *result = lisp_val_from_real(pow(lisp_val_as_real(x), lisp_val_as_real(y)));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_real_pow) {
  return binary_func_with_types(vm, __func__, LISP_REAL, LISP_REAL,
                                do_real_pow);
}

MAKE_REAL_COMPARE(lt, <);
MAKE_REAL_COMPARE(lte, <=);
MAKE_REAL_COMPARE(gt, >);
MAKE_REAL_COMPARE(gte, >=);
MAKE_REAL_COMPARE(eq, ==);

/*
static long int_add(long x, long y) { return x + y; }
static double real_add(double x, double y) { return x + y; }

static long int_mul(long x, long y) { return x * y; }
static double real_mul(double x, double y) { return x * y; }

static long int_sub(long x, long y) { return x - y; }
static double real_sub(double x, double y) { return x - y; }

DEF_BUILTIN(core_add) {
  return arith_nary(vm, __func__, args, lisp_val_from_int(0), int_add, real_add,
                    result);
}

DEF_BUILTIN(core_mul) {
  return arith_nary(vm, __func__, args, lisp_val_from_int(1), int_mul, real_mul,
                    result);
}

static struct lisp_val generic_sub(struct lisp_val x, struct lisp_val y) {
  return generic_arith(int_sub, real_sub, x, y);
}

DEF_BUILTIN(core_sub) {
  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);

  if (!lisp_val_is_number(cell->car)) {
    vm_raise_func_exception(vm, ERROR_INT_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_val total = cell->car;

  // Single argument -> unary negative
  if (lisp_val_type(cell->cdr) == LISP_NIL) {
    *result = generic_sub(lisp_val_from_int(0), total);
    return EV_SUCCESS;
  }

  args = cell->cdr;
  return arith_nary(vm, __func__, args, total, int_sub, real_sub, result);
}

**
 * Generic divide with 0-divisor error checking.
 *
static enum eval_status generic_div(struct lisp_val x, struct lisp_val y,
                                    struct lisp_val *result) {
  enum lisp_type tx = lisp_val_type(x);
  enum lisp_type ty = lisp_val_type(y);
  if (tx == LISP_INT && ty == LISP_INT) {
    // Only check for 0 for integers.
    long divisor = lisp_val_as_int(y);
    if (divisor == 0) {
      vm_raise_func_exception(vm, "cannot divide by 0");
      return EV_EXCEPTION;
    }
    *result = lisp_val_from_int(lisp_val_as_int(x) / divisor);
    return EV_SUCCESS;
  } else {
    *result =
        lisp_val_from_real(lisp_val_to_real(tx, x) / lisp_val_to_real(ty, y));
    return EV_SUCCESS;
  }
}

DEF_BUILTIN(core_div) {
  if (lisp_val_type(args) != LISP_CONS) {
    vm_raise_func_exception(vm, ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);

  struct lisp_val total = cell->car;
  if (!lisp_val_is_number(cell->car)) {
    vm_raise_func_exception(vm, ERROR_INT_ARGS);
    return EV_EXCEPTION;
  }

  args = cell->cdr;

  while (lisp_val_type(args) == LISP_CONS) {
    cell = lisp_val_as_obj(args);
    struct lisp_val divisor = cell->car;
    if (!lisp_val_is_number(divisor)) {
      vm_raise_func_exception(vm, ERROR_INT_ARGS);
      return EV_EXCEPTION;
    }

    if (generic_div(total, divisor, &total) == EV_EXCEPTION) {
      return EV_EXCEPTION;
    }
    args = cell->cdr;
  }

  *result = total;
  return EV_SUCCESS;
}
*/

DEF_BUILTIN(core_identical) {
  DEF_ARG(a, 0);
  DEF_ARG(b, 1);

  BUILTIN_RETURN(lisp_val_identical(a, b) ? lisp_true() : lisp_false());
}

static enum eval_status make_cons(struct lisp_val first, struct lisp_val second,
                                  struct lisp_val *result) {
  *result = lisp_val_from_obj(lisp_cons_create(first, second));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_make_cons) { return binary_func(vm, __func__, make_cons); }

enum eval_status car(struct lisp_val arg, struct lisp_val *result) {
  *result = LISP_VAL_AS(struct lisp_cons, arg)->car;
  return EV_SUCCESS;
}

enum eval_status cdr(struct lisp_val arg, struct lisp_val *result) {
  *result = LISP_VAL_AS(struct lisp_cons, arg)->cdr;
  return EV_SUCCESS;
}

DEF_BUILTIN(core_car) {
  return unary_func_with_type(vm, __func__, LISP_CONS, car);
}

DEF_BUILTIN(core_cdr) {
  return unary_func_with_type(vm, __func__, LISP_CONS, cdr);
}

DEF_BUILTIN(core_string_concat) {
  struct str_builder builder;
  str_builder_init(&builder);

  unsigned arg_count = vm_stack_size(vm);
  for (unsigned i = 0; i < arg_count; i++) {
    struct lisp_val next_arg = vm_from_frame_pointer(vm, i);
    struct lisp_string *s = lisp_val_cast(LISP_STRING, next_arg);
    if (s == NULL) {
      vm_raise_func_exception(vm, ERROR_STRING_ARG);
      goto ERROR;
    }
    str_builder_concat(&builder, s);
  }

  BUILTIN_RETURN(lisp_val_from_obj(str_build(&builder)));

ERROR:
  str_build(&builder);
  return EV_EXCEPTION;
}

enum eval_status string_count(struct lisp_val arg, struct lisp_val *result) {
  *result = lisp_val_from_int(lisp_string_length(lisp_val_as_obj(arg)));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_string_count) {
  return unary_func_with_type(vm, __func__, LISP_STRING, string_count);
}

DEF_BUILTIN(core_string_get) {
  DEF_OBJ_ARG(struct lisp_string, str, LISP_STRING, 0);
  DEF_INT_ARG(index, 1);

  if (index < 0 || (long)lisp_string_length(str) <= index) {
    vm_raise_func_exception(vm, "string index out of bounds: %ld", index);
    return EV_EXCEPTION;
  }

  BUILTIN_RETURN(lisp_val_from_char(lisp_string_get(str, index)));
}

DEF_BUILTIN(core_is_cons) { return type_pred(vm, __func__, LISP_CONS); }

DEF_BUILTIN(core_is_null) { return type_pred(vm, __func__, LISP_NIL); }

DEF_BUILTIN(core_is_integer) { return type_pred(vm, __func__, LISP_INT); }

DEF_BUILTIN(core_is_real) { return type_pred(vm, __func__, LISP_REAL); }

DEF_BUILTIN(core_to_real) {
  DEF_ARG(arg, 0);

  switch (lisp_val_type(arg)) {
    case LISP_INT:
      BUILTIN_RETURN(lisp_val_from_real((double)lisp_val_as_int(arg)));
    case LISP_REAL:
      BUILTIN_RETURN(arg);
    default:
      vm_raise_func_exception(vm, ERROR_NUMBER_ARG);
      return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_to_int) {
  DEF_ARG(arg, 0);

  switch (lisp_val_type(arg)) {
    case LISP_INT:
      BUILTIN_RETURN(arg);
    case LISP_REAL:
      BUILTIN_RETURN(lisp_val_from_int((long)lisp_val_as_real(arg)));
    default:
      vm_raise_func_exception(vm, ERROR_NUMBER_ARG);
      return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_string_to_symbol) {
  DEF_OBJ_ARG(struct lisp_string, str, LISP_STRING, 0);

  const char *name = lisp_string_as_cstr(str);
  if (is_valid_symbol(name)) {
    BUILTIN_RETURN(
        lisp_val_from_obj(lisp_symbol_create(name, lisp_string_length(str))));
  } else {
    vm_raise_func_exception(vm, "invalid symbol name: '%s'", name);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_is_symbol) { return type_pred(vm, __func__, LISP_SYMBOL); }

DEF_BUILTIN(core_is_char) { return type_pred(vm, __func__, LISP_CHAR); }

MAKE_CHAR_COMPARE(lt, <);
MAKE_CHAR_COMPARE(lte, <=);
MAKE_CHAR_COMPARE(gt, >);
MAKE_CHAR_COMPARE(gte, >=);
MAKE_CHAR_COMPARE(eq, ==);

DEF_BUILTIN(core_char_to_int) {
  DEF_CHAR_ARG(c, 0);
  BUILTIN_RETURN(lisp_val_from_int(c));
}

DEF_BUILTIN(core_int_to_char) {
  DEF_INT_ARG(int_val, 0);

  // TODO Expand range when better char support is available
  if (LISP_CHAR_MIN <= int_val && int_val <= LISP_CHAR_MAX) {
    BUILTIN_RETURN(lisp_val_from_char((lisp_char_t)int_val));
  } else {
    vm_raise_func_exception(vm, "integer value out of range: %ld", int_val);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_is_string) { return type_pred(vm, __func__, LISP_STRING); }

static enum eval_status do_string_eq(struct lisp_val x, struct lisp_val y,
                                     struct lisp_val *result) {
  *result = lisp_string_eq(lisp_val_as_obj(x), lisp_val_as_obj(y))
                ? lisp_true()
                : lisp_false();
  return EV_SUCCESS;
}

DEF_BUILTIN(core_string_eq) {
  return binary_func_with_types(vm, __func__, LISP_STRING, LISP_STRING,
                                do_string_eq);
}

DEF_BUILTIN(core_is_function) {
  return unary_pred(vm, __func__, lisp_val_is_func);
}

DEF_BUILTIN(core_is_atom) { return type_pred(vm, __func__, LISP_ATOM); }

static enum eval_status make_atom(struct lisp_val arg,
                                  struct lisp_val *result) {
  *result = lisp_val_from_obj(lisp_atom_create(arg));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_make_atom) { return unary_func(vm, __func__, make_atom); }

static enum eval_status do_deref(struct lisp_val arg, struct lisp_val *result) {
  *result = lisp_atom_deref(lisp_val_as_obj(arg));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_deref) {
  return unary_func_with_type(vm, __func__, LISP_ATOM, do_deref);
}

static enum eval_status do_reset(struct lisp_val atom,
                                 struct lisp_val new_value,
                                 struct lisp_val *result) {
  lisp_atom_reset(lisp_val_as_obj(atom), new_value);
  *result = new_value;
  return EV_SUCCESS;
}

DEF_BUILTIN(core_reset) { return binary_func(vm, __func__, do_reset); }

static enum eval_status do_to_string(struct lisp_val arg,
                                     struct lisp_val *result) {
  *result = lisp_val_from_obj(print_str(arg, false));
  return EV_SUCCESS;
}

/**
 * Convert value to a string.
 */
DEF_BUILTIN(core_to_string) { return unary_func(vm, __func__, do_to_string); }

static enum eval_status do_write_str(struct lisp_val arg,
                                     struct lisp_val *result) {
  *result = lisp_val_from_obj(print_str(arg, true));
  return EV_SUCCESS;
}

/**
 * Convert value to an AST string.
 */
DEF_BUILTIN(core_write_str) { return unary_func(vm, __func__, do_write_str); }

static enum eval_status do_write(struct lisp_val arg, struct lisp_val *result) {
  display_str(print_str(arg, true));
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

/**
 * Print AST.
 */
DEF_BUILTIN(core_write) { return unary_func(vm, __func__, do_write); }

/**
 * Print values with ' ' separator.
 */
DEF_BUILTIN(core_newline) {
  putchar('\n');
  BUILTIN_RETURN(LISP_VAL_NIL);
}

static enum eval_status do_display(struct lisp_val arg,
                                   struct lisp_val *result) {
  display_str(print_str(arg, false));
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

/**
 * Print value in non-readable format
 */
DEF_BUILTIN(core_display) { return unary_func(vm, __func__, do_display); }

DEF_BUILTIN(core_flush) {
  fflush(stdout);
  BUILTIN_RETURN(LISP_VAL_NIL);
}

DEF_BUILTIN(core_read_str) {
  DEF_OBJ_ARG(struct lisp_string, arg, LISP_STRING, 0);
  struct lisp_val result;
  enum parse_res res = read_str(lisp_string_as_cstr(arg), &result);
  if (res == P_SUCCESS) {
    BUILTIN_RETURN(result);
  } else {
    vm_raise_func_exception(vm, "failed to parse string");
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_slurp) {
  DEF_OBJ_ARG(struct lisp_string, filename, LISP_STRING, 0);

  struct lisp_string *contents = read_file(vm, lisp_string_as_cstr(filename));
  if (contents == NULL) {
    // Exception is set by read_file
    return EV_EXCEPTION;
  }

  BUILTIN_RETURN(lisp_val_from_obj(contents));
}

DEF_BUILTIN(core_load_file) {
  DEF_OBJ_ARG(struct lisp_string, filename, LISP_STRING, 0);

  enum eval_status res = load_file(vm, lisp_string_as_cstr(filename));
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    BUILTIN_RETURN(LISP_VAL_NIL);
  }
}

DEF_BUILTIN(core_raise) {
  DEF_ARG(exception, 0);
  vm_raise_exception(vm, exception);
  return EV_EXCEPTION;
}

/*
DEF_BUILTIN(core_with_exception_handler) {
  DEF_ARG(handler, 0);
  DEF_ARG(thunk, 1);

  // TODO Check number of function args
  if (!lisp_val_is_func(handler)) {
    vm_raise_func_exception(vm, "handler must be a function");
    return EV_EXCEPTION;
  }

  // TODO Check number of function args
  if (!lisp_val_is_func(thunk)) {
    vm_raise_func_exception(vm, "thunk must be a function");
    return EV_EXCEPTION;
  }

  struct stack_frame_state saved;
  vm_stack_frame_save(vm, &saved);

  // Call with no arguments
  enum eval_status res = eval_apply(vm, thunk, LISP_VAL_NIL);
  if (res != EV_EXCEPTION) {
    // Apply leaves the return value on the stack
    return res;
  } else {
    assert(vm_has_exception(vm));

    struct lisp_val handler_args = lisp_val_from_obj(
        lisp_cons_create(vm_current_exception(vm), LISP_VAL_NIL));
    vm_clear_exception(vm);

    vm_stack_frame_unwind_to(vm, &saved);

    return eval_apply(vm, handler, handler_args);
  }
}
*/

DEF_BUILTIN(core_runtime) {
  clock_t t = clock();
  // TODO Convert to consistent unit?
  BUILTIN_RETURN(lisp_val_from_int(t));
}

DEF_BUILTIN(core_time_ms) {
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  BUILTIN_RETURN(lisp_val_from_int(ts.tv_sec * 1000 + ts.tv_nsec / 1000));
}

DEF_BUILTIN(core_sleep) {
  DEF_INT_ARG(millis, 0);

  long seconds = millis / 1000;
  long nanos = (millis - seconds * 1000) * 1000;
  struct timespec t = {.tv_sec = seconds, .tv_nsec = nanos};
  if (thrd_sleep(&t, NULL) < 0) {
    vm_raise_func_exception(vm, "sleep interrupted");
    return EV_EXCEPTION;
  } else {
    BUILTIN_RETURN(LISP_VAL_NIL);
  }
}

DEF_BUILTIN(core_macroexpand_1) {
  DEF_ARG(ast, 0);

  struct lisp_cons *ast_cons = lisp_val_cast(LISP_CONS, ast);
  if (ast_cons == NULL) {
    BUILTIN_RETURN(ast);
  }

  struct lisp_symbol *head_sym = lisp_val_cast(LISP_SYMBOL, ast_cons->car);
  if (head_sym == NULL) {
    BUILTIN_RETURN(ast);
  }

  const struct lisp_env_binding *binding =
      lisp_env_get(vm_current_env(vm), head_sym);
  if (binding == NULL || !binding->is_macro) {
    BUILTIN_RETURN(ast);
  }

  return eval_apply(vm, binding->value, ast_cons->cdr);
}

DEF_BUILTIN(core_compile_to_closure) {
  struct lisp_val ast = vm_stack_pop(vm);

  struct lisp_closure *cl = compile_top_level(vm, ast);
  if (cl == NULL) {
    return EV_EXCEPTION;
  }

  BUILTIN_RETURN(lisp_val_from_obj(cl));
}

DEF_BUILTIN(core_prepare_apply) {
  struct lisp_val args = vm_stack_pop(vm);
  struct lisp_val func = vm_stack_pop(vm);
  // TODO Do these need to be GC-preserved? Currently pushing to the stack
  // doesn't trigger GC but that might change in the future

  // TODO Clear the stack instead of asserting?
  assert(vm_stack_size(vm) == 0);

  if (!lisp_val_is_func(func)) {
    vm_raise_func_exception(vm, ERROR_FUNC_ARG);
    return EV_EXCEPTION;
  }

  struct lisp_cons *args_cons;
  while ((args_cons = lisp_val_cast(LISP_CONS, args)) != NULL) {
    vm_stack_push(vm, args_cons->car);
    args = args_cons->cdr;
  }

  // Improper list
  if (!lisp_val_is_nil(args)) {
    vm_raise_func_exception(vm, ERROR_LIST_ARG);
    return EV_EXCEPTION;
  }

  vm_stack_push(vm, func);
  return EV_SUCCESS;
}

typedef enum eval_status (*intrinsic_fn)(struct lisp_vm *vm);

struct builtin_config {
  const char *name;
  intrinsic_fn c_func;
  unsigned req_arg_count;
  bool is_variadic;
};

enum intrinsic_id {
  INTRINSIC_FN_START = 0,

  INTRINSIC_IS_INTEGER = INTRINSIC_FN_START,
  INTRINSIC_TO_INT,
  INTRINSIC_INT_ADD,
  INTRINSIC_INT_SUB,
  INTRINSIC_INT_MUL,
  INTRINSIC_INT_DIV,
  INTRINSIC_INT_BITAND,
  INTRINSIC_INT_BITOR,
  INTRINSIC_INT_BITXOR,
  INTRINSIC_INT_BITSHIFT,

  INTRINSIC_INT_LT,
  INTRINSIC_INT_LTE,
  INTRINSIC_INT_GT,
  INTRINSIC_INT_GTE,
  INTRINSIC_INT_EQ,

  INTRINSIC_IS_REAL,
  INTRINSIC_TO_REAL,
  INTRINSIC_REAL_ADD,
  INTRINSIC_REAL_SUB,
  INTRINSIC_REAL_MUL,
  INTRINSIC_REAL_DIV,
  INTRINSIC_REAL_LT,
  INTRINSIC_REAL_LTE,
  INTRINSIC_REAL_GT,
  INTRINSIC_REAL_GTE,
  INTRINSIC_REAL_EQ,

  INTRINSIC_REAL_EXP,
  INTRINSIC_REAL_LOG,
  INTRINSIC_REAL_POW,

  /* Implemented in the prelude
    INTRINSIC_ADD,
    INTRINSIC_SUB,
    INTRINSIC_MUL,
    INTRINSIC_DIV,
    INTRINSIC_LT,
    INTRINSIC_LTE,
    INTRINSIC_GT,
    INTRINSIC_GTE,
  */

  // TODO Get consistent set of equality checks. Current setup is:
  // - =: compares by reference. Works for integers and symbols due to how
  // they are implemented
  // - equal?: structural comparison, working for strings and lists
  // TODO Make equality checks variadic (this one and others)?
  INTRINSIC_IDENTICAL,
  INTRINSIC_STRING_TO_SYMBOL,
  INTRINSIC_IS_SYMBOL,
  INTRINSIC_IS_FUNCTION,
  INTRINSIC_IS_NULL,
  INTRINSIC_MAKE_CONS,
  INTRINSIC_IS_CONS,
  INTRINSIC_CAR,
  INTRINSIC_CDR,

  INTRINSIC_IS_CHAR,
  // TODO Impl char comparisions via char->int?
  INTRINSIC_CHAR_LT,
  INTRINSIC_CHAR_LTE,
  INTRINSIC_CHAR_GT,
  INTRINSIC_CHAR_GTE,
  INTRINSIC_CHAR_EQ,
  INTRINSIC_CHAR_TO_INT,
  INTRINSIC_INT_TO_CHAR,

  INTRINSIC_IS_STRING,
  INTRINSIC_STRING_EQ,
  INTRINSIC_STRING_COUNT,
  INTRINSIC_STRING_GET,
  INTRINSIC_STRING_CONCAT,
  INTRINSIC_TO_STRING,

  INTRINSIC_IS_ATOM,
  INTRINSIC_MAKE_ATOM,
  INTRINSIC_DEREF,
  INTRINSIC_RESET,

  INTRINSIC_READ_STR,
  INTRINSIC_WRITE,
  INTRINSIC_WRITE_STR,
  INTRINSIC_DISPLAY,
  INTRINSIC_NEWLINE,
  INTRINSIC_FLUSH,
  INTRINSIC_SLURP,
  INTRINSIC_LOAD_FILE,
  INTRINSIC_RAISE,
  // INTRINSIC_WITH_EXCEPTION_HANDLER,
  INTRINSIC_RUNTIME,
  INTRINSIC_TIME_MS,
  INTRINSIC_SLEEP,

  INTRINSIC_MACROEXPAND_1,

  INTRINSIC_FN_END,
  INTRINSIC_INTERNAL_START = INTRINSIC_FN_END,

  INTRINSIC_PERPARE_APPLY = INTRINSIC_INTERNAL_START,
  INTRINSIC_COMPILE_TO_CLOSURE,

  INTRINSIC_INVALID,
};

static_assert(INTRINSIC_INVALID <= UINT8_MAX, "too many intrinsics");

static const struct builtin_config builtins[] = {
    [INTRINSIC_IS_INTEGER] = {"int?", core_is_integer, 1, false},
    [INTRINSIC_TO_INT] = {"int", core_to_int, 1, false},
    [INTRINSIC_INT_ADD] = {"int+", core_int_add, 2, false},
    [INTRINSIC_INT_SUB] = {"int-", core_int_sub, 2, false},
    [INTRINSIC_INT_MUL] = {"int*", core_int_mul, 2, false},
    [INTRINSIC_INT_DIV] = {"int/", core_int_div, 2, false},
    [INTRINSIC_INT_BITAND] = {"bitand", core_int_bitand, 2, false},
    [INTRINSIC_INT_BITOR] = {"bitor", core_int_bitor, 2, false},
    [INTRINSIC_INT_BITXOR] = {"bitxor", core_int_bitxor, 2, false},
    [INTRINSIC_INT_BITSHIFT] = {"bitshift", core_int_bitshift, 2, false},

    [INTRINSIC_INT_LT] = {"int<", core_int_lt, 2, false},
    [INTRINSIC_INT_LTE] = {"int<=", core_int_lte, 2, false},
    [INTRINSIC_INT_GT] = {"int>", core_int_gt, 2, false},
    [INTRINSIC_INT_GTE] = {"int>=", core_int_gte, 2, false},
    [INTRINSIC_INT_EQ] = {"int=", core_int_eq, 2, false},

    [INTRINSIC_IS_REAL] = {"real?", core_is_real, 1, false},
    [INTRINSIC_TO_REAL] = {"real", core_to_real, 1, false},
    [INTRINSIC_REAL_ADD] = {"real+", core_real_add, 2, false},
    [INTRINSIC_REAL_SUB] = {"real-", core_real_sub, 2, false},
    [INTRINSIC_REAL_MUL] = {"real*", core_real_mul, 2, false},
    [INTRINSIC_REAL_DIV] = {"real/", core_real_div, 2, false},
    [INTRINSIC_REAL_LT] = {"real<", core_real_lt, 2, false},
    [INTRINSIC_REAL_LTE] = {"real<=", core_real_lte, 2, false},
    [INTRINSIC_REAL_GT] = {"real>", core_real_gt, 2, false},
    [INTRINSIC_REAL_GTE] = {"real>=", core_real_gte, 2, false},
    [INTRINSIC_REAL_EQ] = {"real=", core_real_eq, 2, false},

    [INTRINSIC_REAL_EXP] = {"e**", core_real_exp, 2, false},
    [INTRINSIC_REAL_LOG] = {"log", core_real_log, 2, false},
    [INTRINSIC_REAL_POW] = {"real**", core_real_pow, 2, false},

    /* Implemented in the prelude
      [INTRINSIC_ADD] = {"+", core_add},
      [INTRINSIC_SUB] = {"-", core_sub},
      [INTRINSIC_MUL] = {"*", core_mul},
      [INTRINSIC_DIV] = {"/", core_div},
      [INTRINSIC_LT] = {"<", core_lt},
      [INTRINSIC_LTE] = {"<=", core_lte},
      [INTRINSIC_GT] = {">", core_gt},
      [INTRINSIC_GTE] = {">=", core_gte},
    */

    // TODO Get consistent set of equality checks. Current setup is:
    // - =: compares by reference. Works for integers and symbols due to how
    // they are implemented
    // - equal?: structural comparison, working for strings and lists
    // TODO Make equality checks variadic (this one and others)?
    [INTRINSIC_IDENTICAL] = {"=", core_identical, 2, false},
    [INTRINSIC_STRING_TO_SYMBOL] = {"string->symbol", core_string_to_symbol, 1,
                                    false},
    [INTRINSIC_IS_SYMBOL] = {"symbol?", core_is_symbol, 1, false},
    [INTRINSIC_IS_FUNCTION] = {"fn?", core_is_function, 1, false},
    [INTRINSIC_IS_NULL] = {"null?", core_is_null, 1, false},
    [INTRINSIC_MAKE_CONS] = {"cons", core_make_cons, 2, false},
    [INTRINSIC_IS_CONS] = {"cons?", core_is_cons, 1, false},
    [INTRINSIC_CAR] = {"car", core_car, 1, false},
    [INTRINSIC_CDR] = {"cdr", core_cdr, 1, false},

    [INTRINSIC_IS_CHAR] = {"char?", core_is_char, 1, false},
    // TODO Impl char comparisions via char->int?
    [INTRINSIC_CHAR_LT] = {"char<", core_char_lt, 2, false},
    [INTRINSIC_CHAR_LTE] = {"char<=", core_char_lte, 2, false},
    [INTRINSIC_CHAR_GT] = {"char>", core_char_gt, 2, false},
    [INTRINSIC_CHAR_GTE] = {"char>=", core_char_gte, 2, false},
    [INTRINSIC_CHAR_EQ] = {"char=", core_char_eq, 2, false},
    [INTRINSIC_CHAR_TO_INT] = {"char->int", core_char_to_int, 1, false},
    [INTRINSIC_INT_TO_CHAR] = {"int->char", core_int_to_char, 1, false},

    [INTRINSIC_IS_STRING] = {"string?", core_is_string, 1, false},
    [INTRINSIC_STRING_EQ] = {"string=", core_string_eq, 2, false},
    [INTRINSIC_STRING_COUNT] = {"string-count", core_string_count, 1, false},
    [INTRINSIC_STRING_GET] = {"string-get", core_string_get, 2, false},
    [INTRINSIC_STRING_CONCAT] = {"string-concat", core_string_concat, 0, true},
    [INTRINSIC_TO_STRING] = {"->string", core_to_string, 1, false},

    [INTRINSIC_IS_ATOM] = {"atom?", core_is_atom, 1, false},
    [INTRINSIC_MAKE_ATOM] = {"atom", core_make_atom, 1, false},
    [INTRINSIC_DEREF] = {"deref", core_deref, 1, false},
    [INTRINSIC_RESET] = {"reset!", core_reset, 2, false},

    [INTRINSIC_READ_STR] = {"read-str", core_read_str, 1, false},
    [INTRINSIC_WRITE] = {"write", core_write, 1, false},
    [INTRINSIC_WRITE_STR] = {"write-str", core_write_str, 1, false},
    [INTRINSIC_DISPLAY] = {"display", core_display, 1, false},
    [INTRINSIC_NEWLINE] = {"newline", core_newline, 0, false},
    [INTRINSIC_FLUSH] = {"flush", core_flush, 0, false},
    [INTRINSIC_SLURP] = {"slurp", core_slurp, 1, false},
    [INTRINSIC_LOAD_FILE] = {"load-file", core_load_file, 1, false},
    [INTRINSIC_RAISE] = {"raise", core_raise, 1, false},
    // [INTRINSIC_WITH_EXCEPTION_HANDLER] = {"with-exception-handler",
    // core_with_exception_handler, 2, false},
    [INTRINSIC_RUNTIME] = {"runtime", core_runtime, 0, false},
    [INTRINSIC_TIME_MS] = {"time-ms", core_time_ms, 0, false},
    [INTRINSIC_SLEEP] = {"sleep", core_sleep, 1, false},

    [INTRINSIC_MACROEXPAND_1] = {"macroexpand-1", core_macroexpand_1, 1, false},

    // These are intrinsics, but not exposed directly as functions
    [INTRINSIC_PERPARE_APPLY] = {NULL, core_prepare_apply, 2, false},
    [INTRINSIC_COMPILE_TO_CLOSURE] = {NULL, core_compile_to_closure, 1, false},
};

#define BUILTIN_COUNT (sizeof(builtins) / sizeof(builtins[0]))

// Not a 100% test, but as good as possible without some macro magic
static_assert(BUILTIN_COUNT == INTRINSIC_INVALID, "missing builtins");

static struct lisp_closure *make_builtin(struct lisp_env *global_env,
                                         const char *name, uint8_t index,
                                         unsigned req_arg_count,
                                         bool is_variadic) {
  struct code_chunk *chunk = chunk_create();
  chunk_append_byte(chunk, OP_INTRINSIC);
  chunk_append_byte(chunk, index);
  chunk_append_byte(chunk, OP_RETURN);

  struct lisp_closure *cl =
      lisp_closure_create(req_arg_count, is_variadic, global_env, chunk);
  gc_push_root_obj(cl);
  lisp_closure_set_name(cl, lisp_symbol_create_cstr(name));
  gc_pop_root_expect_obj(cl);
  return cl;
}

static struct lisp_closure *make_builtin_apply(void) {
  struct code_chunk *code = chunk_create();
  chunk_append_byte(code, OP_INTRINSIC);
  chunk_append_byte(code, INTRINSIC_PERPARE_APPLY);
  chunk_append_byte(code, OP_TAIL_CALL);

  struct lisp_closure *cl = lisp_closure_create(2, false, NULL, code);
  gc_push_root_obj(cl);
  struct lisp_symbol *name = lisp_symbol_create_cstr("apply");
  lisp_closure_set_name(cl, name);
  gc_pop_root_expect_obj(cl);
  return cl;
}

static struct lisp_closure *make_builtin_eval(void) {
  // TODO Expose `(compile)` and implement `(eval)` in the prelude?
  struct code_chunk *code = chunk_create();
  chunk_append_byte(code, OP_INTRINSIC);
  chunk_append_byte(code, INTRINSIC_COMPILE_TO_CLOSURE);
  chunk_append_byte(code, OP_TAIL_CALL);

  struct lisp_closure *cl = lisp_closure_create(1, false, NULL, code);
  gc_push_root_obj(cl);
  struct lisp_symbol *name = lisp_symbol_create_cstr("eval");
  lisp_closure_set_name(cl, name);
  gc_pop_root_expect_obj(cl);
  return cl;
}

static struct lisp_closure *make_builtin_with_exception_handler() {
  struct code_chunk *code = chunk_create();
  chunk_append_byte(code, OP_SET_EX_HANDLER);
  unsigned handler_offset_cell = chunk_append_byte(code, 0);
  chunk_append_byte(code, 0);
  // Fetch and call the thunk
  chunk_append_byte(code, OP_DUP_FP);
  chunk_append_byte(code, 1);
  // Separate call + return since the stack frame needs to stay intact for the
  // exception handler
  chunk_append_byte(code, OP_CALL);
  chunk_append_byte(code, 0);  // no args
  chunk_append_byte(code, OP_RETURN);

  // In the exception handler code, the exception will be pushed on the stack
  // Fetch and call the handler
  unsigned handler_offset = chunk_append_byte(code, OP_DUP_FP);
  chunk_append_byte(code, 0);
  chunk_append_byte(code, OP_SKIP_CLEAR);
  chunk_append_byte(code, 2);  // Handler function and exception
  chunk_append_byte(code, OP_TAIL_CALL);

  // TODO Use helper functions from the compiler
  // TODO Make this special syntax handled by the compiler?
  unsigned branch_offset = handler_offset - handler_offset_cell;
  chunk_set_byte(code, handler_offset_cell, branch_offset & 0xff);
  chunk_set_byte(code, handler_offset_cell + 1, branch_offset >> 8);

  struct lisp_closure *cl = lisp_closure_create(2, false, NULL, code);
  gc_push_root_obj(cl);
  struct lisp_symbol *name = lisp_symbol_create_cstr("with-exception-handler");
  lisp_closure_set_name(cl, name);
  gc_pop_root_expect_obj(cl);
  return cl;
}

enum eval_status call_intrinsic(uint8_t index, struct lisp_vm *vm) {
  assert(index < BUILTIN_COUNT);
  return builtins[index].c_func(vm);
}

static void define_const(struct lisp_env *env, const char *name,
                         struct lisp_val v) {
  gc_push_root(v);
  lisp_env_set(env, lisp_symbol_create_cstr(name), v);
  gc_pop_root_expect(v);
}

static void define_cl(struct lisp_env *env, struct lisp_closure *cl) {
  struct lisp_symbol *name = lisp_closure_name(cl);
  assert(name != NULL);
  lisp_env_set(env, name, lisp_val_from_obj(cl));
}

void define_builtins(struct lisp_env *global_env) {
  for (unsigned i = INTRINSIC_FN_START; i < INTRINSIC_FN_END; i++) {
    const struct builtin_config *b = &builtins[i];
    define_cl(global_env, make_builtin(global_env, b->name, i, b->req_arg_count,
                                       b->is_variadic));
  }

  // Special builtins
  define_cl(global_env, make_builtin_apply());
  define_cl(global_env, make_builtin_eval());
  define_cl(global_env, make_builtin_with_exception_handler());

  // TODO Make these constants or part of the reader?
  define_const(global_env, "true", lisp_true());
  define_const(global_env, "false", lisp_false());
}
