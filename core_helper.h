#ifndef CORE_HELPER_H_
#define CORE_HELPER_H_

#include "eval.h"
#include "types.h"
#include "vm.h"

#define DEF_BUILTIN(c_name) static enum eval_status c_name(struct lisp_vm *vm)

#define DEF_ARG(arg, index) \
  struct lisp_val arg = vm_from_frame_pointer(vm, (index));

#define DEF_PRIM_ARG(decl_type, arg, lisp_type, cast_name, index)           \
  DEF_ARG(_raw_arg_##arg, index)                                            \
  if (lisp_val_type(_raw_arg_##arg) != (lisp_type)) {                       \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, lisp_type_name(lisp_type)); \
    return EV_EXCEPTION;                                                    \
  }                                                                         \
  decl_type arg = lisp_val_as_##cast_name(_raw_arg_##arg);                  \
  (void)0

#define DEF_INT_ARG(arg, index) DEF_PRIM_ARG(long, arg, LISP_INT, int, index)
#define DEF_CHAR_ARG(arg, index) \
  DEF_PRIM_ARG(lisp_char_t, arg, LISP_CHAR, char, index)
#define DEF_REAL_ARG(arg, index) \
  DEF_PRIM_ARG(double, arg, LISP_REAL, real, index)

#define DEF_OBJ_ARG(decl_type, arg, lisp_type, index)                       \
  decl_type *arg =                                                          \
      lisp_val_cast((lisp_type), vm_from_frame_pointer(vm, (index)));       \
  if (arg == NULL) {                                                        \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, lisp_type_name(lisp_type)); \
    return EV_EXCEPTION;                                                    \
  }                                                                         \
  (void)0

#define BUILTIN_RETURN(val) \
  vm_stack_push(vm, (val)); \
  return EV_SUCCESS

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

#define raise_helper_exception(message, ...)    \
  vm_raise_format_exception(vm, "%s: " message, \
                            func_name __VA_OPT__(, ) __VA_ARGS__)

#define MAKE_NUM_BINARY(prefix, lisp_type, name, op)                   \
  static enum eval_status do_##prefix##_##name(                        \
      struct lisp_val x, struct lisp_val y, struct lisp_val *result) { \
    *result = lisp_val_from_##prefix(lisp_val_as_##prefix(x)           \
                                         op lisp_val_as_##prefix(y));  \
    return EV_SUCCESS;                                                 \
  }                                                                    \
  DEF_BUILTIN(core_##prefix##_##name) {                                \
    return binary_func_with_types(vm, __func__, lisp_type, lisp_type,  \
                                  do_##prefix##_##name);               \
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
    return binary_func_with_types(vm, __func__, lisp_type, lisp_type,  \
                                  do_##prefix##_##name);               \
  }

#define MAKE_INT_BINARY(name, op) MAKE_NUM_BINARY(int, LISP_INT, name, op)
#define MAKE_INT_COMPARE(name, op) MAKE_NUM_COMPARE(int, LISP_INT, name, op)

#define MAKE_REAL_BINARY(name, op) MAKE_NUM_BINARY(real, LISP_REAL, name, op)
#define MAKE_REAL_COMPARE(name, op) MAKE_NUM_COMPARE(real, LISP_REAL, name, op)

#define MAKE_CHAR_COMPARE(name, op) MAKE_NUM_COMPARE(char, LISP_CHAR, name, op)

struct lisp_val generic_arith(long (*int_op)(long x, long y),
                              double (*real_op)(double x, double y),
                              struct lisp_val x, struct lisp_val y);
enum eval_status arith_compare(struct lisp_vm *vm, const char *func_name,
                               bool (*compare)(long a, long b));

enum eval_status type_pred(struct lisp_vm *vm, const char *func_name,
                           enum lisp_type type);
enum eval_status unary_pred(struct lisp_vm *vm, const char *func_name,
                            bool (*pred)(struct lisp_val arg));

enum eval_status unary_func(struct lisp_vm *vm, const char *func_name,
                            enum eval_status (*transform)(
                                struct lisp_val arg, struct lisp_val *result));
enum eval_status unary_func_with_type(
    struct lisp_vm *vm, const char *func_name, enum lisp_type required_type,
    enum eval_status (*transform)(struct lisp_val arg,
                                  struct lisp_val *result));
enum eval_status binary_func(
    struct lisp_vm *vm, const char *func_name,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result));
enum eval_status binary_func_with_types(
    struct lisp_vm *vm, const char *func_name, enum lisp_type a_type,
    enum lisp_type b_type,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result));

#endif
