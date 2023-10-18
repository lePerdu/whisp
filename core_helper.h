#ifndef CORE_HELPER_H_
#define CORE_HELPER_H_

#include "eval.h"
#include "types.h"
#include "vm.h"

#define DEF_BUILTIN(c_name) static enum eval_status c_name(struct lisp_vm *vm)

#define POP_ARG(arg) struct lisp_val arg = vm_stack_pop(vm)

#define POP_TYPED_ARG(decl_type, arg, pred, conv)                           \
  POP_ARG(_raw_arg_##arg);                                                  \
  if (!pred(_raw_arg_##arg)) {                                              \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, LISP_TYPE_NAME(decl_type)); \
    return EV_EXCEPTION;                                                    \
  }                                                                         \
  decl_type arg = conv(_raw_arg_##arg);                                     \
  (void)0

#define POP_OBJ_ARG(obj_type, arg, pred) \
  POP_TYPED_ARG(obj_type *, arg, pred, lisp_val_as_obj)

#define POP_INT_ARG(arg) \
  POP_TYPED_ARG(long, arg, lisp_val_is_int, lisp_val_as_int)
#define POP_REAL_ARG(arg) \
  POP_TYPED_ARG(double, arg, lisp_val_is_int, lisp_val_as_int)
#define POP_CHAR_ARG(arg) \
  POP_TYPED_ARG(lisp_char_t, arg, lisp_val_is_char, lisp_val_as_char)

#define REF_ARG(arg, index) \
  struct lisp_val arg = vm_from_stack_pointer(vm, (index))

#define REF_TYPED_ARG(decl_type, arg, pred, conv, index)                    \
  REF_ARG(_raw_arg_##arg, (index));                                         \
  if (!(pred)(_raw_arg_##arg)) {                                            \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, LISP_TYPE_NAME(decl_type)); \
    return EV_EXCEPTION;                                                    \
  }                                                                         \
  decl_type arg = (conv)(_raw_arg_##arg);                                   \
  (void)0

#define REF_OBJ_ARG(obj_type, arg, pred, index) \
  REF_TYPED_ARG(obj_type *, arg, pred, lisp_val_as_obj, index)

#define REF_INT_ARG(arg, index) \
  REF_TYPED_ARG(long, arg, lisp_val_is_int, lisp_val_as_int, index)
#define REF_REAL_ARG(arg, index) \
  REF_TYPED_ARG(double, arg, lisp_val_is_int, lisp_val_as_int, index)
#define REF_CHAR_ARG(arg, index) \
  REF_TYPED_ARG(lisp_char_t, arg, lisp_val_is_char, lisp_val_as_char, index)

#define CLEAR_ARGS(n)                                    \
  do {                                                   \
    for (int counter = (n)-1; counter >= 0; counter--) { \
      vm_stack_pop(vm);                                  \
    }                                                    \
  } while (false)

#define BUILTIN_RETURN(val) \
  vm_stack_push(vm, (val)); \
  return EV_SUCCESS;        \
  (void)0

#define BUILTIN_RETURN_OBJ(val)              \
  vm_stack_push(vm, lisp_val_from_obj(val)); \
  return EV_SUCCESS;                         \
  (void)0

#define DEF_BUILTIN_PRED(name, pred)                            \
  DEF_BUILTIN(name) {                                           \
    BUILTIN_RETURN(lisp_val_from_bool(pred(vm_stack_pop(vm)))); \
  }

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

#define MAKE_NUM_BINARY(prefix, name, decl_type, op)                         \
  DEF_BUILTIN(core_##prefix##_##name) {                                      \
    POP_TYPED_ARG(decl_type, y, lisp_val_is_##prefix, lisp_val_as_##prefix); \
    POP_TYPED_ARG(decl_type, x, lisp_val_is_##prefix, lisp_val_as_##prefix); \
    BUILTIN_RETURN(lisp_val_from_##prefix(x op y));                          \
  }

#define MAKE_NUM_COMPARE(prefix, name, decl_type, op)                        \
  DEF_BUILTIN(core_##prefix##_##name) {                                      \
    POP_TYPED_ARG(decl_type, y, lisp_val_is_##prefix, lisp_val_as_##prefix); \
    POP_TYPED_ARG(decl_type, x, lisp_val_is_##prefix, lisp_val_as_##prefix); \
    BUILTIN_RETURN(lisp_val_from_bool(x op y));                              \
  }

#define MAKE_INT_BINARY(name, op) MAKE_NUM_BINARY(int, name, long, op)
#define MAKE_INT_COMPARE(name, op) MAKE_NUM_COMPARE(int, name, long, op)

#define MAKE_REAL_BINARY(name, op) MAKE_NUM_BINARY(real, name, double, op)
#define MAKE_REAL_COMPARE(name, op) MAKE_NUM_COMPARE(real, name, double, op)

#define MAKE_CHAR_COMPARE(name, op) \
  MAKE_NUM_COMPARE(char, name, lisp_char_t, op)

#endif
