#ifndef CORE_HELPER_H_
#define CORE_HELPER_H_

#include "eval.h"
#include "types.h"

#define DEF_BUILTIN(c_name) \
  static enum eval_status c_name(struct lisp_val args, struct lisp_val *result)

#define ERROR_INT_ARGS "arguments must be of type int"
#define ERROR_REAL_ARGS "arguments must be of type real"
#define ERROR_NUMBER_ARG "argument must be of type number"
#define ERROR_CONS_ARG "argument must be of type cons"
#define ERROR_LIST_ARG "argument must be a list"
#define ERROR_STRING_ARG "argument must be of type string"
#define ERROR_FUNC_ARG "argument must be a function"
#define ERROR_ARG_TYPE "argument must be of type %s"
#define ERROR_NOT_ENOUGH_ARGS "not enough arguments"
#define ERROR_TOO_MANY_ARGS "too many arguments"

#define set_helper_exception(message, ...) \
  set_format_exception("%s: " message, func_name __VA_OPT__(, ) __VA_ARGS__)

#define MAKE_NUM_BINARY(prefix, lisp_type, name, op)                   \
  static enum eval_status do_##prefix##_##name(                        \
      struct lisp_val x, struct lisp_val y, struct lisp_val *result) { \
    *result = lisp_val_from_##prefix(lisp_val_as_##prefix(x)           \
                                         op lisp_val_as_##prefix(y));  \
    return EV_SUCCESS;                                                 \
  }                                                                    \
  DEF_BUILTIN(core_##prefix##_##name) {                                \
    return binary_func_with_types(__func__, lisp_type, lisp_type,      \
                                  do_##prefix##_##name, args, result); \
  }

#define MAKE_NUM_COMPARE(prefix, lisp_type, name, op)                  \
  static enum eval_status do_##prefix##_##name(                        \
      struct lisp_val x, struct lisp_val y, struct lisp_val *result) { \
    *result = lisp_val_as_##prefix(x) op lisp_val_as_##prefix(y)       \
                  ? lisp_true()                                        \
                  : lisp_false();                                      \
    return EV_SUCCESS;                                                 \
  }                                                                    \
  DEF_BUILTIN(core_##prefix##_##name) {                                \
    return binary_func_with_types(__func__, lisp_type, lisp_type,      \
                                  do_##prefix##_##name, args, result); \
  }

#define MAKE_INT_BINARY(name, op) MAKE_NUM_BINARY(int, LISP_INT, name, op)
#define MAKE_INT_COMPARE(name, op) MAKE_NUM_COMPARE(int, LISP_INT, name, op)

#define MAKE_REAL_BINARY(name, op) MAKE_NUM_BINARY(real, LISP_REAL, name, op)
#define MAKE_REAL_COMPARE(name, op) MAKE_NUM_COMPARE(real, LISP_REAL, name, op)

#define MAKE_CHAR_COMPARE(name, op) MAKE_NUM_COMPARE(char, LISP_CHAR, name, op)

struct lisp_val generic_arith(long (*int_op)(long x, long y),
                              double (*real_op)(double x, double y),
                              struct lisp_val x, struct lisp_val y);
enum eval_status arith_nary(const char *func_name, struct lisp_val args,
                            struct lisp_val initial,
                            long (*int_op)(long x, long y),
                            double (*real_op)(double x, double y),
                            struct lisp_val *result);
enum eval_status arith_compare(const char *func_name, struct lisp_val args,
                               bool (*compare)(long a, long b),
                               struct lisp_val *result);
enum eval_status type_pred(const char *func_name, enum lisp_type type,
                           struct lisp_val args, struct lisp_val *result);
enum eval_status unary_pred(const char *func_name,
                            bool (*pred)(struct lisp_val arg),
                            struct lisp_val args, struct lisp_val *result);
enum eval_status unary_func(
    const char *func_name,
    enum eval_status (*transform)(struct lisp_val arg, struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result);
enum eval_status unary_func_with_type(
    const char *func_name, enum lisp_type required_type,
    enum eval_status (*transform)(struct lisp_val arg, struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result);
enum eval_status binary_func(
    const char *func_name,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result);
enum eval_status binary_func_with_types(
    const char *func_name, enum lisp_type a_type, enum lisp_type b_type,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result),
    struct lisp_val args, struct lisp_val *result);

#endif
