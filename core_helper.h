#ifndef CORE_HELPER_H_
#define CORE_HELPER_H_

#include "eval.h"
#include "types.h"
#include "vm.h"

#define DEF_BUILTIN(c_name) static enum eval_status c_name(struct lisp_vm *vm)

#define DEF_ARG(arg, index) \
  struct lisp_val arg = vm_from_frame_pointer(vm, (index));

#define DEF_PRIM_ARG(decl_type, arg, pred, conv, index)                     \
  DEF_ARG(_raw_arg_##arg, index)                                            \
  if (!pred(_raw_arg_##arg)) {                                              \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, LISP_TYPE_NAME(decl_type)); \
    return EV_EXCEPTION;                                                    \
  }                                                                         \
  decl_type arg = conv(_raw_arg_##arg);                                     \
  (void)0

#define DEF_INT_ARG(arg, index) \
  DEF_PRIM_ARG(long, arg, lisp_val_is_int, lisp_val_as_int, index)
#define DEF_CHAR_ARG(arg, index) \
  DEF_PRIM_ARG(lisp_char_t, arg, lisp_val_is_char, lisp_val_as_char, index)
#define DEF_REAL_ARG(arg, index) \
  DEF_PRIM_ARG(double, arg, lisp_val_is_real, lisp_val_as_real, index)

#define DEF_OBJ_ARG(decl_type, arg, pred, index)                              \
  DEF_ARG(_raw_arg_##arg, index);                                             \
  if (!pred(_raw_arg_##arg)) {                                                \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, LISP_TYPE_NAME(decl_type *)); \
    return EV_EXCEPTION;                                                      \
  }                                                                           \
  decl_type *arg = lisp_val_as_obj(_raw_arg_##arg);                           \
  (void)0

#define POP_ARG(arg) struct lisp_val arg = vm_stack_pop(vm)

#define POP_OBJ_ARG(decl_type, arg, pred)                                     \
  decl_type *arg = lisp_val_cast(pred, vm_stack_pop(vm));                     \
  if (arg == NULL) {                                                          \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, LISP_TYPE_NAME(decl_type *)); \
    return EV_EXCEPTION;                                                      \
  }                                                                           \
  (void)0

#define BUILTIN_RETURN(val) \
  vm_stack_push(vm, (val)); \
  return EV_SUCCESS;        \
  (void)0

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

#define MAKE_NUM_BINARY(prefix, name, decl_type, op)                           \
  DEF_BUILTIN(core_##prefix##_##name) {                                        \
    DEF_PRIM_ARG(decl_type, x, lisp_val_is_##prefix, lisp_val_as_##prefix, 0); \
    DEF_PRIM_ARG(decl_type, y, lisp_val_is_##prefix, lisp_val_as_##prefix, 1); \
    BUILTIN_RETURN(lisp_val_from_##prefix(x op y));                            \
  }

#define MAKE_NUM_COMPARE(prefix, name, decl_type, op)                          \
  DEF_BUILTIN(core_##prefix##_##name) {                                        \
    DEF_PRIM_ARG(decl_type, x, lisp_val_is_##prefix, lisp_val_as_##prefix, 0); \
    DEF_PRIM_ARG(decl_type, y, lisp_val_is_##prefix, lisp_val_as_##prefix, 1); \
    BUILTIN_RETURN((x op y) ? lisp_true() : lisp_false());                     \
  }

#define MAKE_INT_BINARY(name, op) MAKE_NUM_BINARY(int, name, long, op)
#define MAKE_INT_COMPARE(name, op) MAKE_NUM_COMPARE(int, name, long, op)

#define MAKE_REAL_BINARY(name, op) MAKE_NUM_BINARY(real, name, double, op)
#define MAKE_REAL_COMPARE(name, op) MAKE_NUM_COMPARE(real, name, double, op)

#define MAKE_CHAR_COMPARE(name, op) \
  MAKE_NUM_COMPARE(char, name, lisp_char_t, op)

struct lisp_val generic_arith(long (*int_op)(long x, long y),
                              double (*real_op)(double x, double y),
                              struct lisp_val x, struct lisp_val y);
enum eval_status arith_compare(struct lisp_vm *vm, const char *func_name,
                               bool (*compare)(long a, long b));

enum eval_status unary_pred(struct lisp_vm *vm, const char *func_name,
                            lisp_predicate pred);

enum eval_status unary_func(struct lisp_vm *vm, const char *func_name,
                            enum eval_status (*transform)(
                                struct lisp_val arg, struct lisp_val *result));
enum eval_status binary_func(
    struct lisp_vm *vm, const char *func_name,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result));

#endif
