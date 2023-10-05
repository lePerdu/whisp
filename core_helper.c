#include "core_helper.h"

#include "types.h"
#include "vm.h"

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

// TODO Improve error messages using func_name

enum eval_status type_pred(struct lisp_vm *vm, const char *func_name,
                           enum lisp_type type) {
  DEF_ARG(single_arg, 0);
  (void)func_name;

  BUILTIN_RETURN(lisp_val_type(single_arg) == type ? lisp_true()
                                                   : lisp_false());
}

enum eval_status unary_pred(struct lisp_vm *vm, const char *func_name,
                            bool (*pred)(struct lisp_val arg)) {
  (void)func_name;
  BUILTIN_RETURN(pred(vm_stack_pop(vm)) ? lisp_true() : lisp_false());
}

enum eval_status unary_func(struct lisp_vm *vm, const char *func_name,
                            enum eval_status (*transform)(
                                struct lisp_val arg, struct lisp_val *result)) {
  struct lisp_val single_arg = vm_stack_top(vm);
  (void)func_name;

  struct lisp_val result;
  enum eval_status res = transform(single_arg, &result);
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    vm_stack_pop(vm);
    BUILTIN_RETURN(result);
  }
}

enum eval_status unary_func_with_type(
    struct lisp_vm *vm, const char *func_name, enum lisp_type required_type,
    enum eval_status (*transform)(struct lisp_val arg,
                                  struct lisp_val *result)) {
  struct lisp_val single_arg = vm_stack_top(vm);

  if (lisp_val_type(single_arg) != required_type) {
    raise_helper_exception(ERROR_ARG_TYPE, lisp_type_name(required_type));
    return EV_EXCEPTION;
  }

  struct lisp_val result;
  enum eval_status res = transform(single_arg, &result);
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    vm_stack_pop(vm);
    BUILTIN_RETURN(result);
  }
}

enum eval_status binary_func(
    struct lisp_vm *vm, const char *func_name,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result)) {
  struct lisp_val first = vm_from_stack_pointer(vm, 1);
  struct lisp_val second = vm_stack_top(vm);
  (void)func_name;

  struct lisp_val result;
  enum eval_status res = transform(first, second, &result);
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    vm_stack_pop(vm);
    vm_stack_pop(vm);
    BUILTIN_RETURN(result);
  }
}

enum eval_status binary_func_with_types(
    struct lisp_vm *vm, const char *func_name, enum lisp_type a_type,
    enum lisp_type b_type,
    enum eval_status (*transform)(struct lisp_val a, struct lisp_val b,
                                  struct lisp_val *result)) {
  struct lisp_val first = vm_from_stack_pointer(vm, 1);
  struct lisp_val second = vm_stack_top(vm);
  (void)func_name;

  if (lisp_val_type(first) != a_type) {
    raise_helper_exception(ERROR_ARG_TYPE, lisp_type_name(a_type));
    return EV_EXCEPTION;
  }

  if (lisp_val_type(second) != b_type) {
    raise_helper_exception(ERROR_ARG_TYPE, lisp_type_name(b_type));
    return EV_EXCEPTION;
  }

  struct lisp_val result;
  enum eval_status res = transform(first, second, &result);
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    BUILTIN_RETURN(result);
  }
}
