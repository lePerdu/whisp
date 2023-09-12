#include "core_helper.h"

static double lisp_val_to_real(enum lisp_type type, struct lisp_val x) {
  switch (type) {
  case LISP_INT:
    return (double)lisp_val_as_int(x);
  case LISP_REAL:
    return lisp_val_as_real(x);
  default:
    assert(false);
    return 0.0;
  }
}

struct lisp_val generic_arith(long (*int_op)(long x, long y),
                              double (*real_op)(double x, double y),
                              struct lisp_val x, struct lisp_val y) {
  enum lisp_type tx = lisp_val_type(x);
  enum lisp_type ty = lisp_val_type(y);
  if (tx == LISP_INT && ty == LISP_INT) {
    return lisp_val_from_int(int_op(lisp_val_as_int(x), lisp_val_as_int(y)));
  } else {
    return lisp_val_from_real(
        real_op(lisp_val_to_real(tx, x), lisp_val_to_real(ty, y)));
  }
}

enum eval_status arith_nary(const char *func_name, struct lisp_val args,
                            struct lisp_val initial,
                            long (*int_op)(long x, long y),
                            double (*real_op)(double x, double y),
                            struct lisp_val *result) {
  struct lisp_val total = initial;

  while (lisp_val_type(args) == LISP_CONS) {
    struct lisp_cons *cell = lisp_val_as_obj(args);
    if (!lisp_val_is_number(cell->car)) {
      set_helper_exception(ERROR_NUMBER_ARG);
      return EV_EXCEPTION;
    }

    total = generic_arith(int_op, real_op, total, cell->car);
    args = cell->cdr;
  }

  *result = total;
  return EV_SUCCESS;
}

enum eval_status arith_compare(const char *func_name, struct lisp_val args,
                               bool (*compare)(long a, long b),
                               struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    *result = lisp_true();
    return EV_SUCCESS;
  }

  struct lisp_cons *cell = lisp_val_as_obj(args);

  if (lisp_val_type(cell->car) != LISP_INT) {
    set_helper_exception(ERROR_INT_ARGS);
    return EV_EXCEPTION;
  }

  long current = lisp_val_as_int(cell->car);

  args = cell->cdr;

  while (lisp_val_type(args) == LISP_CONS) {
    cell = lisp_val_as_obj(args);
    if (lisp_val_type(cell->car) != LISP_INT) {
      set_helper_exception(ERROR_INT_ARGS);
      return EV_EXCEPTION;
    }

    long next = lisp_val_as_int(cell->car);
    if (!compare(current, next)) {
      *result = lisp_false();
      return EV_SUCCESS;
    }
    current = next;
    args = cell->cdr;
  }

  *result = lisp_true();
  return EV_SUCCESS;
}

#define MAKE_ARITH_COMPARE(name, op)                                           \
  static bool name(long a, long b) { return a op b; }                          \
  DEF_BUILTIN(core_##name) {                                                   \
    return arith_compare(__func__, args, name, result);                        \
  }

enum eval_status type_pred(const char *func_name, enum lisp_type type,
                           struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val single_arg = args_cons->car;

  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }

  if (lisp_val_type(single_arg) == type) {
    *result = lisp_true();
  } else {
    *result = LISP_VAL_NIL;
  }
  return EV_SUCCESS;
}

enum eval_status unary_pred(const char *func_name,
                            bool (*pred)(struct lisp_val arg),
                            struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS, func_name);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val single_arg = args_cons->car;

  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_helper_exception(ERROR_TOO_MANY_ARGS, func_name);
    return EV_EXCEPTION;
  }

  if (pred(single_arg)) {
    *result = lisp_true();
  } else {
    *result = LISP_VAL_NIL;
  }
  return EV_SUCCESS;
}

enum eval_status unary_func(
    const char *func_name,
    enum eval_status (*transform)(struct lisp_val arg, struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val single_arg = args_cons->car;

  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_helper_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  return transform(single_arg, result);
}

enum eval_status unary_func_with_type(
    const char *func_name, enum lisp_type required_type,
    enum eval_status (*transform)(struct lisp_val arg, struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val single_arg = args_cons->car;

  if (lisp_val_type(single_arg) != required_type) {
    set_helper_exception(ERROR_ARG_TYPE, lisp_type_name(required_type));
    return EV_EXCEPTION;
  }

  if (lisp_val_type(args_cons->cdr) != LISP_NIL) {
    set_helper_exception(ERROR_TOO_MANY_ARGS);
    return EV_EXCEPTION;
  }

  return transform(single_arg, result);
}

enum eval_status
binary_func(const char *func_name,
            enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                          struct lisp_val *result),
            struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val first = args_cons->car;
  args = args_cons->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS, func_name);
    return EV_EXCEPTION;
  }
  args_cons = lisp_val_as_obj(args);
  struct lisp_val second = args_cons->car;
  args = args_cons->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    set_helper_exception(ERROR_TOO_MANY_ARGS, func_name);
    return EV_EXCEPTION;
  }

  return transform(first, second, result);
}

enum eval_status binary_func_with_types(
    const char *func_name, enum lisp_type a_type, enum lisp_type b_type,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result) {
  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS);
    return EV_EXCEPTION;
  }
  struct lisp_cons *args_cons = lisp_val_as_obj(args);
  struct lisp_val first = args_cons->car;
  if (lisp_val_type(first) != a_type) {
    set_helper_exception(ERROR_ARG_TYPE, lisp_type_name(a_type));
    return EV_EXCEPTION;
  }
  args = args_cons->cdr;

  if (lisp_val_type(args) != LISP_CONS) {
    set_helper_exception(ERROR_NOT_ENOUGH_ARGS, func_name);
    return EV_EXCEPTION;
  }
  args_cons = lisp_val_as_obj(args);
  struct lisp_val second = args_cons->car;
  if (lisp_val_type(second) != b_type) {
    set_helper_exception(ERROR_ARG_TYPE, lisp_type_name(b_type));
    return EV_EXCEPTION;
  }
  args = args_cons->cdr;

  if (lisp_val_type(args) != LISP_NIL) {
    set_helper_exception(ERROR_TOO_MANY_ARGS, func_name);
    return EV_EXCEPTION;
  }

  return transform(first, second, result);
}
