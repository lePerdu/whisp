#include "eval.h"

#include <stdbool.h>
#include <stdint.h>

#include "bytecode.h"
#include "core.h"
#include "env.h"
#include "memory.h"
#include "symbol.h"
#include "types.h"
#include "vm.h"

static inline uint8_t chunk_read_byte(struct code_chunk *chunk, unsigned *ip) {
  assert(*ip < chunk->bytecode.size);
  uint8_t b = chunk->bytecode.data[*ip];
  (*ip)++;
  return b;
}

static inline int16_t chunk_read_short(struct code_chunk *chunk, unsigned *ip) {
  return (uint16_t)chunk_read_byte(chunk, ip) +
         ((uint16_t)chunk_read_byte(chunk, ip) << 8);
}

static enum eval_status build_rest_args(struct lisp_vm *vm, unsigned from_fp) {
  int count = vm_stack_size(vm) - from_fp;
  if (count < 0) {
    vm_raise_format_exception(vm,
                              "cannot build rest args: not enough arguments");
    return EV_EXCEPTION;
  }

  vm_stack_push(vm, LISP_VAL_NIL);
  for (int i = 0; i < count; i++) {
    struct lisp_val cdr = vm_stack_pop(vm);
    struct lisp_val car = vm_stack_pop(vm);
    vm_stack_push(vm, lisp_val_from_obj(lisp_cons_create(car, cdr)));
  }
  return EV_SUCCESS;
}

static enum eval_status check_call_args(struct lisp_vm *vm,
                                        struct lisp_closure *func,
                                        unsigned arg_count) {
  unsigned req_args = func->code->req_arg_count;
  if (arg_count < req_args) {
    vm_raise_format_exception(vm, "%s: not enough arguments",
                              lisp_closure_name_cstr(func));
    return EV_EXCEPTION;
  }
  if (arg_count > req_args && !func->code->is_variadic) {
    vm_raise_format_exception(vm, "%s: too many arguments",
                              lisp_closure_name_cstr(func));
    return EV_EXCEPTION;
  }

  return EV_SUCCESS;
}

/**
 * Typecheck and argcheck a function object.
 *
 * If there is an error, an exception is raised and `NULL` is returned.
 */
static struct lisp_closure *check_call(struct lisp_vm *vm,
                                       struct lisp_val func_obj,
                                       unsigned arg_count) {
  struct lisp_closure *new_func = lisp_val_cast(lisp_val_is_func, func_obj);
  if (new_func == NULL) {
    vm_raise_format_exception(vm, "cannot call value of type: %s",
                              lisp_val_type_name(func_obj));
    return NULL;
  }

  if (check_call_args(vm, new_func, arg_count) == EV_EXCEPTION) {
    return NULL;
  }

  return new_func;
}

/**
 * Evaluate bytecode until the call stack is empty.
 */
static enum eval_status eval_bytecode(struct lisp_vm *vm) {
  assert(vm_current_frame_index(vm) == 1);

  struct stack_frame *frame;
  struct code_chunk *code;
  unsigned *ip;

  enum bytecode_op op;

LOOP_NEW_FRAME:
  frame = vm_current_frame(vm);
  code = frame->func->code;
  ip = &frame->instr_pointer;
  assert(*ip < code->bytecode.size);

LOOP:
  op = chunk_read_byte(code, ip);

  // TODO Error checking before the asserts (including in VM functions)
  switch (op) {
    case OP_CONST: {
      uint8_t const_idx = chunk_read_byte(code, ip);
      assert(const_idx < code->const_table.size);
      vm_stack_push(vm, code->const_table.data[const_idx]);
      goto LOOP;
    }
    case OP_GET_FP: {
      uint8_t idx = chunk_read_byte(code, ip);
      vm_stack_push(vm, vm_from_frame_pointer(vm, idx));
      goto LOOP;
    }
    case OP_GET_UPVALUE: {
      uint8_t idx = chunk_read_byte(code, ip);
      vm_stack_push(vm, lisp_closure_get_capture(frame->func, idx));
      goto LOOP;
    }
    case OP_GET_GLOBAL: {
      uint8_t const_idx = chunk_read_byte(code, ip);
      assert(const_idx < code->const_table.size);
      struct lisp_val const_val = code->const_table.data[const_idx];
      if (!lisp_val_is_env_binding(const_val)) {
        vm_raise_format_exception(
            vm, "argument to get-global must be an environment binding");
        goto HANDLE_FATAL_ERROR;
      }
      struct lisp_env_binding *binding = lisp_val_as_obj(const_val);
      struct lisp_val value = lisp_env_binding_value(binding);
      if (lisp_is_uninitialized(value)) {
        // This is one of the few non-fatal errors in the evaluator
        vm_raise_format_exception(
            vm, "symbol not bound: %s",
            lisp_symbol_name(lisp_env_binding_name(binding)));
        goto HANDLE_EXCEPTION;
      }
      vm_stack_push(vm, value);
      goto LOOP;
    }
    case OP_SET_GLOBAL: {
      uint8_t const_idx = chunk_read_byte(code, ip);
      assert(const_idx < code->const_table.size);
      struct lisp_val const_val = code->const_table.data[const_idx];
      if (!lisp_val_is_env_binding(const_val)) {
        vm_raise_format_exception(
            vm, "argument to set-global must be an environment binding");
        goto HANDLE_FATAL_ERROR;
      }
      struct lisp_env_binding *binding = lisp_val_as_obj(const_val);
      lisp_env_binding_set_value(binding, vm_stack_pop(vm));
      goto LOOP;
    }
    case OP_DUP:
      vm_stack_push(vm, vm_stack_top(vm));
      goto LOOP;
    case OP_POP:
      vm_stack_pop(vm);
      goto LOOP;
    case OP_SKIP_DELETE: {
      uint8_t skip_n = chunk_read_byte(code, ip);
      uint8_t del_n = chunk_read_byte(code, ip);
      vm_stack_frame_skip_delete(vm, skip_n, del_n);
      goto LOOP;
    }
    case OP_CALL: {
      uint8_t arg_count = chunk_read_byte(code, ip);
      struct lisp_val func_val = vm_stack_pop(vm);
      struct lisp_closure *func = check_call(vm, func_val, arg_count);
      if (func == NULL) {
        goto HANDLE_EXCEPTION;
      }
      vm_create_stack_frame(vm, func, arg_count);
      goto LOOP_NEW_FRAME;
    }
    case OP_TAIL_CALL: {
      struct lisp_val func_val = vm_stack_pop(vm);
      unsigned arg_count = vm_stack_size(vm);
      struct lisp_closure *func = check_call(vm, func_val, arg_count);
      if (func == NULL) {
        goto HANDLE_EXCEPTION;
      }
      vm_replace_stack_frame(vm, func);
      goto LOOP_NEW_FRAME;
    }
    case OP_RETURN:
      vm_stack_frame_return(vm);
      if (vm_current_frame_index(vm) == 0) {
        // Nothing left to evaluate
        return EV_SUCCESS;
      } else {
        goto LOOP_NEW_FRAME;
      }
    case OP_ALLOC_CLOSURE: {
      uint8_t const_idx = chunk_read_byte(code, ip);
      uint8_t n_captures = chunk_read_byte(code, ip);
      assert(const_idx < code->const_table.size);
      struct lisp_val code_val = code->const_table.data[const_idx];
      if (!is_chunk(code_val)) {
        vm_raise_format_exception(vm,
                                  "cannot make closure with code of type: %s",
                                  lisp_val_type_name(code_val));
        goto HANDLE_FATAL_ERROR;
      }

      struct lisp_closure *cl =
          lisp_closure_create(lisp_val_as_obj(code_val), n_captures);
      vm_stack_push(vm, lisp_val_from_obj(cl));
      goto LOOP;
    }
    case OP_INIT_CLOSURE: {
      uint8_t n_captures = chunk_read_byte(code, ip);
      assert(vm_stack_size(vm) >= (unsigned)(n_captures + 1));
      struct lisp_val after_captures = vm_from_stack_pointer(vm, n_captures);
      struct lisp_closure *closure =
          lisp_val_cast(lisp_val_is_func, after_captures);
      if (closure == NULL) {
        vm_raise_format_exception(
            vm, "cannot initialize object of type %s as closure",
            lisp_val_type_name(after_captures));
        goto HANDLE_FATAL_ERROR;
      }

      if (closure->n_captures != n_captures) {
        vm_raise_format_exception(vm,
                                  "tried to initialize closure with wrong "
                                  "number of captures. Expected %u, got %u",
                                  closure->n_captures, n_captures);
        goto HANDLE_FATAL_ERROR;
      }

      for (int i = n_captures - 1; i >= 0; i--) {
        lisp_closure_set_capture(closure, i, vm_stack_pop(vm));
      }
      // Leave closure on top of the stack as if returned
      goto LOOP;
    }
    case OP_BUILD_REST_ARGS: {
      uint8_t start_fp = chunk_read_byte(code, ip);
      enum eval_status res = build_rest_args(vm, start_fp);
      if (res == EV_EXCEPTION) {
        goto HANDLE_FATAL_ERROR;
      }
      goto LOOP;
    }
    case OP_BRANCH: {
      int16_t offset = chunk_read_short(code, ip);
      int new_ip = *ip + offset - 2;
      if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
        vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                  new_ip);
        goto HANDLE_FATAL_ERROR;
      }
      *ip = new_ip;
      goto LOOP;
    }
    case OP_BRANCH_IF_FALSE: {
      int16_t offset = chunk_read_short(code, ip);
      if (lisp_val_is_false(vm_stack_pop(vm))) {
        int new_ip = *ip + offset - 2;
        if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
          vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                    new_ip);
          goto HANDLE_FATAL_ERROR;
        }
        *ip = new_ip;
      }
      goto LOOP;
    }
    case OP_INTRINSIC: {
      uint8_t index = chunk_read_byte(code, ip);
      enum eval_status res = call_intrinsic(index, vm);
      if (res == EV_EXCEPTION) {
        if (vm->is_fatal_error) {
          goto HANDLE_FATAL_ERROR;
        } else {
          goto HANDLE_EXCEPTION;
        }
      }
      goto LOOP;
    }
    case OP_GET_CURRENT_FRAME:
      vm_stack_push(vm, lisp_val_from_int(vm_current_frame_index(vm)));
      goto LOOP;
    case OP_RETURN_FROM_FRAME: {
      struct lisp_val frame_val = vm_stack_pop(vm);
      if (!lisp_val_is_int(frame_val)) {
        vm_raise_format_exception(vm, "escape frame must be of type int");
        goto HANDLE_FATAL_ERROR;
      }
      lisp_int_t frame_index = lisp_val_as_int(frame_val);
      if (frame_index <= 0 || vm_current_frame_index(vm) < frame_index) {
        vm_raise_format_exception(vm, "escape frame is out of bounds: %d",
                                  frame_index);
        goto HANDLE_FATAL_ERROR;
      }

      vm_stack_frame_return_from(vm, frame_index);
      if (vm_current_frame_index(vm) == 0) {
        // Nothing left to evaluate
        return EV_SUCCESS;
      } else {
        goto LOOP_NEW_FRAME;
      }
    }
  }

  // Unhandled opcode (other instructions should continue to the top of the
  // loop)
  vm_raise_format_exception(vm, "unknown opcode: %d", op);
  // And then handle the error...

HANDLE_FATAL_ERROR:
  // Many errors in the evaluator can only happen due to compiler bugs, so they
  // cause immediate exit.
  // Unwind the stack, but ignore exception handlers
  while (vm_current_frame_index(vm) > 0) {
    vm_stack_frame_unwind(vm);
  }
  return EV_EXCEPTION;

HANDLE_EXCEPTION : {
  // Manual call to the global error handler
  // TODO Can this process be simplified since this sort of error can only be
  // generated internally?
  // TODO Change error calling-convention to pass a (well-known) error name so
  // the handler can be smarter
  if (lisp_val_is_nil(vm->primitive_error_handler)) {
    goto HANDLE_FATAL_ERROR;
  }
  const int arg_count = 1;
  struct lisp_closure *func =
      check_call(vm, vm->primitive_error_handler, arg_count);
  if (func == NULL) {
    // Missing/invalid handler
    goto HANDLE_FATAL_ERROR;
  }

  // Clear the exception now that the function is checked
  vm_stack_push(vm, vm_current_exception(vm));
  vm_clear_exception(vm);
  vm_create_stack_frame(vm, func, arg_count);
  goto LOOP_NEW_FRAME;
}
}

enum eval_status eval_closure(struct lisp_vm *vm, struct lisp_closure *cl) {
  enum eval_status res = check_call_args(vm, cl, 0);
  if (res == EV_EXCEPTION) {
    return res;
  }

  vm_create_stack_frame(vm, cl, 0);
  return eval_bytecode(vm);
}

enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func_val,
                            struct lisp_val args) {
  unsigned arg_count = 0;
  while (!lisp_val_is_nil(args)) {
    struct lisp_cons *args_cons = lisp_val_cast(lisp_val_is_cons, args);
    if (args_cons == NULL) {
      vm_raise_format_exception(vm, "cannot apply improper list");
      return EV_EXCEPTION;
    }

    vm_stack_push(vm, args_cons->car);
    args = args_cons->cdr;
    arg_count++;
  }

  struct lisp_closure *func = check_call(vm, func_val, arg_count);
  if (func == NULL) {
    return EV_EXCEPTION;
  }

  vm_create_stack_frame(vm, func, arg_count);
  return eval_bytecode(vm);
}
