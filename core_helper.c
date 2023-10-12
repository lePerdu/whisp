#include "core_helper.h"

#include "types.h"
#include "vm.h"

static double lisp_val_to_real(bool is_int, struct lisp_val x) {
  if (is_int) {
    return (double)lisp_val_as_int(x);
  } else {
    return lisp_val_as_real(x);
  }
}

struct lisp_val generic_arith(long (*int_op)(long x, long y),
                              double (*real_op)(double x, double y),
                              struct lisp_val x, struct lisp_val y) {
  bool x_is_int = lisp_val_is_int(x);
  bool y_is_int = lisp_val_is_int(y);
  if (x_is_int && y_is_int) {
    return lisp_val_from_int(int_op(lisp_val_as_int(x), lisp_val_as_int(y)));
  } else {
    return lisp_val_from_real(
        real_op(lisp_val_to_real(x_is_int, x), lisp_val_to_real(x_is_int, y)));
  }
}

// TODO Improve error messages using func_name

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
