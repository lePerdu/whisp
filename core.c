#include "core.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <time.h>

#include "core_helper.h"
#include "eval.h"
#include "file.h"
#include "printer.h"
#include "reader.h"
#include "types.h"

MAKE_INT_BINARY(add, +);
MAKE_INT_BINARY(sub, -);
MAKE_INT_BINARY(mul, *);

static enum eval_status do_int_div(struct lisp_val x, struct lisp_val y,
                                   struct lisp_val *result) {
  long dividend = lisp_val_as_int(x);
  long divisor = lisp_val_as_int(y);
  if (divisor == 0) {
    set_format_exception("cannot divide by 0");
    return EV_EXCEPTION;
  }
  *result = lisp_val_from_int(dividend / divisor);
  return EV_SUCCESS;
}

DEF_BUILTIN(core_int_div) {
  return binary_func_with_types(__func__, LISP_INT, LISP_INT, do_int_div, args,
                                result);
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

static enum eval_status core_real_exp(struct lisp_val args,
                                      struct lisp_val *result) {
  return unary_func_with_type(__func__, LISP_REAL, do_real_exp, args, result);
};

static enum eval_status do_real_log(struct lisp_val x,
                                    struct lisp_val *result) {
  *result = lisp_val_from_real(log(lisp_val_as_real(x)));
  return EV_SUCCESS;
}

static enum eval_status core_real_log(struct lisp_val args,
                                      struct lisp_val *result) {
  return unary_func_with_type(__func__, LISP_REAL, do_real_log, args, result);
};

static enum eval_status do_real_pow(struct lisp_val x, struct lisp_val y,
                                    struct lisp_val *result) {
  *result = lisp_val_from_real(pow(lisp_val_as_real(x), lisp_val_as_real(y)));
  return EV_SUCCESS;
}

static enum eval_status core_real_pow(struct lisp_val args,
                                      struct lisp_val *result) {
  return binary_func_with_types(__func__, LISP_REAL, LISP_REAL, do_real_pow,
                                args, result);
};

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
  return arith_nary(__func__, args, lisp_val_from_int(0), int_add, real_add,
                    result);
}

DEF_BUILTIN(core_mul) {
  return arith_nary(__func__, args, lisp_val_from_int(1), int_mul, real_mul,
                    result);
}

static struct lisp_val generic_sub(struct lisp_val x, struct lisp_val y) {
  return generic_arith(int_sub, real_sub, x, y);
}

DEF_BUILTIN(core_sub) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);

  if (!lisp_val_is_number(cell->car)) {
    set_func_exception(ERROR_INT_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_val total = cell->car;

  // Single argument -> unary negative
  if (lisp_val_type(cell->cdr) == LISP_NIL) {
    *result = generic_sub(lisp_val_from_int(0), total);
    return EV_SUCCESS;
  }

  args = cell->cdr;
  return arith_nary(__func__, args, total, int_sub, real_sub, result);
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
      set_format_exception("cannot divide by 0");
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
    set_func_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);

  struct lisp_val total = cell->car;
  if (!lisp_val_is_number(cell->car)) {
    set_format_exception(ERROR_INT_ARGS);
    return EV_EXCEPTION;
  }

  args = cell->cdr;

  while (lisp_val_type(args) == LISP_CONS) {
    cell = lisp_val_as_obj(args);
    struct lisp_val divisor = cell->car;
    if (!lisp_val_is_number(divisor)) {
      set_func_exception(ERROR_INT_ARGS);
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
  if (lisp_val_type(args) != LISP_CONS) {
    *result = lisp_true();
    return EV_SUCCESS;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);
  // All must be equal to the first
  struct lisp_val first = cell->car;

  args = cell->cdr;

  while (lisp_val_type(args) == LISP_CONS) {
    cell = lisp_val_as_obj(args);

    if (!lisp_val_identical(first, cell->car)) {
      *result = lisp_false();
      return EV_SUCCESS;
    }
    args = cell->cdr;
  }

  *result = lisp_true();
  return EV_SUCCESS;
}

static enum eval_status make_cons(struct lisp_val first, struct lisp_val second,
                                  struct lisp_val *result) {
  *result = lisp_val_from_obj(lisp_cons_create(first, second));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_make_cons) {
  return binary_func(__func__, make_cons, args, result);
}

enum eval_status car(struct lisp_val arg, struct lisp_val *result) {
  *result = LISP_VAL_AS(struct lisp_cons, arg)->car;
  return EV_SUCCESS;
}

enum eval_status cdr(struct lisp_val arg, struct lisp_val *result) {
  *result = LISP_VAL_AS(struct lisp_cons, arg)->cdr;
  return EV_SUCCESS;
}

DEF_BUILTIN(core_car) {
  return unary_func_with_type(__func__, LISP_CONS, car, args, result);
}

DEF_BUILTIN(core_cdr) {
  return unary_func_with_type(__func__, LISP_CONS, cdr, args, result);
}

DEF_BUILTIN(core_string_concat) {
  struct str_builder builder;
  str_builder_init(&builder);

  enum eval_status res = EV_SUCCESS;
  while (!lisp_val_is_nil(args)) {
    struct lisp_cons *cell = lisp_val_as_obj(args);
    args = cell->cdr;

    struct lisp_string *s = lisp_val_cast(LISP_STRING, cell->car);
    if (s == NULL) {
      set_func_exception(ERROR_STRING_ARG);
      res = EV_EXCEPTION;
      break;
    }
    str_builder_concat(&builder, s);
  }

  *result = lisp_val_from_obj(str_build(&builder));
  return res;
}

enum eval_status string_count(struct lisp_val arg, struct lisp_val *result) {
  *result = lisp_val_from_int(lisp_string_length(lisp_val_as_obj(arg)));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_string_count) {
  return unary_func_with_type(__func__, LISP_STRING, string_count, args,
                              result);
}

static enum eval_status string_get(struct lisp_val str_val,
                                   struct lisp_val index_val,
                                   struct lisp_val *result) {
  struct lisp_string *str = lisp_val_as_obj(str_val);
  long index = lisp_val_as_int(index_val);

  if (index < 0 || (long)lisp_string_length(str) <= index) {
    set_func_exception("string index out of bounds: %ld", index);
    return EV_EXCEPTION;
  }

  *result = lisp_val_from_char(lisp_string_get(str, index));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_string_get) {
  return binary_func_with_types(__func__, LISP_STRING, LISP_INT, string_get,
                                args, result);
}

DEF_BUILTIN(core_is_cons) {
  return type_pred(__func__, LISP_CONS, args, result);
}

DEF_BUILTIN(core_is_null) {
  return type_pred(__func__, LISP_NIL, args, result);
}

DEF_BUILTIN(core_is_integer) {
  return type_pred(__func__, LISP_INT, args, result);
}

DEF_BUILTIN(core_is_real) {
  return type_pred(__func__, LISP_REAL, args, result);
}

enum eval_status convert_to_real(struct lisp_val arg, struct lisp_val *result) {
  switch (lisp_val_type(arg)) {
    case LISP_INT:
      *result = lisp_val_from_real((double)lisp_val_as_int(arg));
      return EV_SUCCESS;
    case LISP_REAL:
      *result = arg;
      return EV_SUCCESS;
    default:
      set_func_exception(ERROR_NUMBER_ARG);
      return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_to_real) {
  return unary_func(__func__, convert_to_real, args, result);
}

enum eval_status convert_to_int(struct lisp_val arg, struct lisp_val *result) {
  switch (lisp_val_type(arg)) {
    case LISP_INT:
      *result = arg;
      return EV_SUCCESS;
    case LISP_REAL:
      *result = lisp_val_from_int((long)lisp_val_as_real(arg));
      return EV_SUCCESS;
    default:
      set_func_exception(ERROR_NUMBER_ARG);
      return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_to_int) {
  return unary_func(__func__, convert_to_int, args, result);
}

static enum eval_status string_to_symbol(struct lisp_val arg,
                                         struct lisp_val *result) {
  if (lisp_val_type(arg) != LISP_STRING) {
    set_format_exception(ERROR_STRING_ARG);
    return EV_EXCEPTION;
  }
  struct lisp_string *str = lisp_val_as_obj(arg);
  const char *name = lisp_string_as_cstr(str);
  if (is_valid_symbol(name)) {
    *result =
        lisp_val_from_obj(lisp_symbol_create(name, lisp_string_length(str)));
    return EV_SUCCESS;
  } else {
    set_format_exception("invalid symbol name: '%s'", name);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_string_to_symbol) {
  return unary_func(__func__, string_to_symbol, args, result);
}

DEF_BUILTIN(core_is_symbol) {
  return type_pred(__func__, LISP_SYMBOL, args, result);
}

DEF_BUILTIN(core_is_char) {
  return type_pred(__func__, LISP_CHAR, args, result);
}

MAKE_CHAR_COMPARE(lt, <);
MAKE_CHAR_COMPARE(lte, <=);
MAKE_CHAR_COMPARE(gt, >);
MAKE_CHAR_COMPARE(gte, >=);
MAKE_CHAR_COMPARE(eq, ==);

static enum eval_status do_char_to_int(struct lisp_val arg,
                                       struct lisp_val *result) {
  *result = lisp_val_from_int(lisp_val_as_char(arg));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_char_to_int) {
  return unary_func_with_type(__func__, LISP_CHAR, do_char_to_int, args,
                              result);
}

static enum eval_status do_int_to_char(struct lisp_val arg,
                                       struct lisp_val *result) {
  // TODO Expand range when better char support is available
  long int_val = lisp_val_as_int(arg);
  if (LISP_CHAR_MIN <= int_val && int_val <= LISP_CHAR_MAX) {
    *result = lisp_val_from_char((lisp_char_t)int_val);
    return EV_SUCCESS;
  } else {
    set_func_exception("integer value out of range: %ld", int_val);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_int_to_char) {
  return unary_func_with_type(__func__, LISP_INT, do_int_to_char, args, result);
}

DEF_BUILTIN(core_is_string) {
  return type_pred(__func__, LISP_STRING, args, result);
}

static enum eval_status do_string_eq(struct lisp_val x, struct lisp_val y,
                                     struct lisp_val *result) {
  *result = lisp_string_eq(lisp_val_as_obj(x), lisp_val_as_obj(y))
                ? lisp_true()
                : lisp_false();
  return EV_SUCCESS;
}

static enum eval_status core_string_eq(struct lisp_val args,
                                       struct lisp_val *result) {
  return binary_func_with_types(__func__, LISP_STRING, LISP_STRING,
                                do_string_eq, args, result);
}

DEF_BUILTIN(core_is_function) {
  return unary_pred(__func__, lisp_val_is_func, args, result);
}

DEF_BUILTIN(core_is_atom) {
  return type_pred(__func__, LISP_ATOM, args, result);
}

static enum eval_status make_atom(struct lisp_val arg,
                                  struct lisp_val *result) {
  *result = lisp_val_from_obj(lisp_atom_create(arg));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_make_atom) {
  return unary_func(__func__, make_atom, args, result);
}

static enum eval_status do_deref(struct lisp_val arg, struct lisp_val *result) {
  *result = lisp_atom_deref(lisp_val_as_obj(arg));
  return EV_SUCCESS;
}

DEF_BUILTIN(core_deref) {
  return unary_func_with_type(__func__, LISP_ATOM, do_deref, args, result);
}

static enum eval_status do_reset(struct lisp_val atom,
                                 struct lisp_val new_value,
                                 struct lisp_val *result) {
  lisp_atom_reset(lisp_val_as_obj(atom), new_value);
  *result = new_value;
  return EV_SUCCESS;
}

DEF_BUILTIN(core_reset) {
  return binary_func(__func__, do_reset, args, result);
}

DEF_BUILTIN(core_eval) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_format_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val ast = args_cons->car;

  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_format_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  // TODO Enable TCO for builtins
  return eval(ast, global_env, result);
}

static enum eval_status do_apply(struct lisp_val func, struct lisp_val args,
                                 struct lisp_val *result) {
  if (!lisp_val_is_func(func)) {
    set_format_exception(ERROR_FUNC_ARG);
    return EV_EXCEPTION;
  }
  if (!lisp_val_is_list(args)) {
    set_format_exception(ERROR_LIST_ARG);
    return EV_EXCEPTION;
  }
  return eval_apply(func, args, result);
}

DEF_BUILTIN(core_apply) {
  return binary_func(__func__, do_apply, args, result);
}

static enum eval_status do_to_string(struct lisp_val arg,
                                     struct lisp_val *result) {
  *result = lisp_val_from_obj(print_str(arg, false));
  return EV_SUCCESS;
}

/**
 * Convert value to a string.
 */
DEF_BUILTIN(core_to_string) {
  return unary_func(__func__, do_to_string, args, result);
}

static enum eval_status do_write_str(struct lisp_val arg,
                                     struct lisp_val *result) {
  *result = lisp_val_from_obj(print_str(arg, true));
  return EV_SUCCESS;
}

/**
 * Convert value to an AST string.
 */
DEF_BUILTIN(core_write_str) {
  return unary_func(__func__, do_write_str, args, result);
}

static enum eval_status do_write(struct lisp_val arg, struct lisp_val *result) {
  display_str(print_str(arg, true));
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

/**
 * Print AST.
 */
DEF_BUILTIN(core_write) { return unary_func(__func__, do_write, args, result); }

/**
 * Print values with ' ' separator.
 */
DEF_BUILTIN(core_newline) {
  if (!lisp_val_is_nil(args)) {
    set_func_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }
  putchar('\n');
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
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
DEF_BUILTIN(core_display) {
  return unary_func(__func__, do_display, args, result);
}

DEF_BUILTIN(core_flush) {
  if (!lisp_val_is_nil(args)) {
    set_func_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  fflush(stdout);
  *result = LISP_VAL_NIL;
  return EV_SUCCESS;
}

static enum eval_status do_read_str(struct lisp_val arg,
                                    struct lisp_val *result) {
  enum parse_res res =
      read_str(lisp_string_as_cstr(lisp_val_as_obj(arg)), result);
  if (res == P_SUCCESS) {
    return EV_SUCCESS;
  } else {
    set_format_exception("failed to parse string");
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_read_str) {
  return unary_func_with_type(__func__, LISP_STRING, do_read_str, args, result);
}

static enum eval_status do_slurp(struct lisp_val arg, struct lisp_val *result) {
  struct lisp_string *filename = lisp_val_as_obj(arg);

  struct lisp_string *contents = read_file(lisp_string_as_cstr(filename));
  if (contents == NULL) {
    // Exception is set by read_file
    return EV_EXCEPTION;
  }

  *result = lisp_val_from_obj(contents);
  return EV_SUCCESS;
}

DEF_BUILTIN(core_slurp) {
  return unary_func_with_type(__func__, LISP_STRING, do_slurp, args, result);
}

static enum eval_status do_load_file(struct lisp_val arg,
                                     struct lisp_val *result) {
  struct lisp_string *filename = lisp_val_as_obj(arg);

  *result = LISP_VAL_NIL;
  return load_file(lisp_string_as_cstr(filename));
}

DEF_BUILTIN(core_load_file) {
  return unary_func_with_type(__func__, LISP_STRING, do_load_file, args,
                              result);
}

static enum eval_status do_raise(struct lisp_val arg, struct lisp_val *result) {
  current_exception = arg;
  *result = LISP_VAL_NIL;
  return EV_EXCEPTION;
}

DEF_BUILTIN(core_raise) { return unary_func(__func__, do_raise, args, result); }

DEF_BUILTIN(core_with_exception_handler) {
  struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
  if (args_cons == NULL) {
    set_func_exception("missing guard clause");
    return EV_EXCEPTION;
  }
  struct lisp_val handler = args_cons->car;
  // TODO Check number of function args
  if (!lisp_val_is_func(handler)) {
    set_func_exception("handler must be a function");
    return EV_EXCEPTION;
  }

  args_cons = lisp_val_cast(LISP_CONS, args_cons->cdr);
  if (lisp_val_type(args) != LISP_CONS) {
    set_func_exception("missing thunk");
    return EV_EXCEPTION;
  }
  struct lisp_val thunk = args_cons->car;
  // TODO Check number of function args
  if (!lisp_val_is_func(thunk)) {
    set_func_exception("thunk must be a function");
    return EV_EXCEPTION;
  }

  if (!lisp_val_is_nil(args_cons->cdr)) {
    set_func_exception("too many arguments");
    return EV_EXCEPTION;
  }

  // Call with no arguments
  enum eval_status res = eval_apply(thunk, LISP_VAL_NIL, result);
  if (res != EV_EXCEPTION) {
    return res;
  } else {
    struct lisp_val handler_args =
        lisp_val_from_obj(lisp_cons_create(current_exception, LISP_VAL_NIL));
    current_exception = LISP_VAL_NIL;
    return eval_apply(handler, handler_args, result);
  }
}

DEF_BUILTIN(core_runtime) {
  if (!lisp_val_is_nil(args)) {
    set_func_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  clock_t t = clock();
  // TODO Convert to consistent unit?
  *result = lisp_val_from_int(t);
  return EV_SUCCESS;
}

DEF_BUILTIN(core_time_ms) {
  if (!lisp_val_is_nil(args)) {
    set_func_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  *result = lisp_val_from_int(ts.tv_sec * 1000 + ts.tv_nsec / 1000);
  return EV_SUCCESS;
}

enum eval_status do_sleep(struct lisp_val arg, struct lisp_val *result) {
  long millis = lisp_val_as_int(arg);
  long seconds = millis / 1000;
  long nanos = (millis - seconds * 1000) * 1000;
  struct timespec t = {.tv_sec = seconds, .tv_nsec = nanos};
  if (thrd_sleep(&t, NULL) < 0) {
    set_func_exception("sleep interrupted");
    return EV_EXCEPTION;
  } else {
    *result = LISP_VAL_NIL;
    return EV_SUCCESS;
  }
}

DEF_BUILTIN(core_sleep) {
  return unary_func_with_type(__func__, LISP_INT, do_sleep, args, result);
}

struct lisp_builtin builtins[] = {
    lisp_builtin_make("int?", core_is_integer, 1, false),
    lisp_builtin_make("int", core_to_int, 1, false),
    lisp_builtin_make("int+", core_int_add, 2, false),
    lisp_builtin_make("int-", core_int_sub, 2, false),
    lisp_builtin_make("int*", core_int_mul, 2, false),
    lisp_builtin_make("int/", core_int_div, 2, false),
    lisp_builtin_make("int<", core_int_lt, 2, false),
    lisp_builtin_make("int<=", core_int_lte, 2, false),
    lisp_builtin_make("int>", core_int_gt, 2, false),
    lisp_builtin_make("int>=", core_int_gte, 2, false),
    lisp_builtin_make("int=", core_int_eq, 2, false),

    lisp_builtin_make("real?", core_is_real, 1, false),
    lisp_builtin_make("real", core_to_real, 1, false),
    lisp_builtin_make("real+", core_real_add, 2, false),
    lisp_builtin_make("real-", core_real_sub, 2, false),
    lisp_builtin_make("real*", core_real_mul, 2, false),
    lisp_builtin_make("real/", core_real_div, 2, false),
    lisp_builtin_make("real<", core_real_lt, 2, false),
    lisp_builtin_make("real<=", core_real_lte, 2, false),
    lisp_builtin_make("real>", core_real_gt, 2, false),
    lisp_builtin_make("real>=", core_real_gte, 2, false),
    lisp_builtin_make("real=", core_real_eq, 2, false),

    lisp_builtin_make("e**", core_real_exp, 2, false),
    lisp_builtin_make("log", core_real_log, 2, false),
    lisp_builtin_make("real**", core_real_pow, 2, false),

    /* Implemented in the prelude
      lisp_builtin_make("+", core_add),
      lisp_builtin_make("-", core_sub),
      lisp_builtin_make("*", core_mul),
      lisp_builtin_make("/", core_div),
      lisp_builtin_make("<", core_lt),
      lisp_builtin_make("<=", core_lte),
      lisp_builtin_make(">", core_gt),
      lisp_builtin_make(">=", core_gte),
    */

    // TODO Get consistent set of equality checks. Current setup is:
    // - =: compares by reference. Works for integers and symbols due to how
    // they are implemented
    // - equal?: structural comparison, working for strings and lists
    // TODO Make equality checks consistent. Either:
    // - Make = binary
    // - Make other checks variadic
    lisp_builtin_make("=", core_identical, 0, true),
    lisp_builtin_make("string->symbol", core_string_to_symbol, 1, false),
    lisp_builtin_make("symbol?", core_is_symbol, 1, false),
    lisp_builtin_make("fn?", core_is_function, 1, false),
    lisp_builtin_make("null?", core_is_null, 1, false),
    lisp_builtin_make("cons", core_make_cons, 2, false),
    lisp_builtin_make("cons?", core_is_cons, 1, false),
    lisp_builtin_make("car", core_car, 1, false),
    lisp_builtin_make("cdr", core_cdr, 1, false),

    lisp_builtin_make("char?", core_is_char, 1, false),
    lisp_builtin_make("char<", core_char_lt, 2, false),
    lisp_builtin_make("char<=", core_char_lte, 2, false),
    lisp_builtin_make("char>", core_char_gt, 2, false),
    lisp_builtin_make("char>=", core_char_gte, 2, false),
    lisp_builtin_make("char=", core_char_eq, 2, false),
    lisp_builtin_make("char->int", core_char_to_int, 1, false),
    lisp_builtin_make("int->char", core_int_to_char, 1, false),

    lisp_builtin_make("string?", core_is_string, 1, false),
    lisp_builtin_make("string=", core_string_eq, 2, false),
    lisp_builtin_make("string-count", core_string_count, 1, false),
    lisp_builtin_make("string-get", core_string_get, 2, false),
    lisp_builtin_make("string-concat", core_string_concat, 0, true),
    lisp_builtin_make("->string", core_to_string, 1, false),

    lisp_builtin_make("atom?", core_is_atom, 1, false),
    lisp_builtin_make("atom", core_make_atom, 1, false),
    lisp_builtin_make("deref", core_deref, 1, false),
    lisp_builtin_make("reset!", core_reset, 2, false),

    lisp_builtin_make("eval", core_eval, 1, false),
    lisp_builtin_make("apply", core_apply, 2, false),
    lisp_builtin_make("read-str", core_read_str, 1, false),
    lisp_builtin_make("write", core_write, 1, false),
    lisp_builtin_make("write-str", core_write_str, 1, false),
    lisp_builtin_make("display", core_display, 1, false),
    lisp_builtin_make("newline", core_newline, 1, false),
    lisp_builtin_make("flush", core_flush, 0, false),
    lisp_builtin_make("slurp", core_slurp, 1, false),
    lisp_builtin_make("load-file", core_load_file, 1, false),
    lisp_builtin_make("raise", core_raise, 1, false),
    lisp_builtin_make("with-exception-handler", core_with_exception_handler, 2,
                      false),
    lisp_builtin_make("runtime", core_runtime, 0, false),
    lisp_builtin_make("time-ms", core_time_ms, 0, false),
    lisp_builtin_make("sleep", core_sleep, 1, false),

    lisp_builtin_make(NULL, NULL, 0, false),
};
