#include "eval.h"

#include <stdbool.h>
#include <stdint.h>

#include "bytecode.h"
#include "core.h"
#include "memory.h"
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
  unsigned req_args = lisp_closure_arg_count(func);
  if (arg_count < req_args) {
    vm_raise_format_exception(vm, "not enough arguments");
    return EV_EXCEPTION;
  }
  if (arg_count > req_args && !lisp_closure_is_variadic(func)) {
    vm_raise_format_exception(vm, "too many arguments");
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
  struct lisp_closure *new_func = lisp_val_cast(LISP_CLOSURE, func_obj);
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
  const unsigned initial_frame = vm_current_frame_index(vm);

  while (true) {
    struct stack_frame *frame = vm_current_frame(vm);
    struct code_chunk *code = lisp_closure_code(frame->func);
    unsigned *ip = &frame->instr_pointer;
    assert(*ip < code->bytecode.size);

    enum bytecode_op op = chunk_read_byte(code, ip);

    // TODO Error checking before the asserts (including in VM functions)
    switch (op) {
      case OP_CONST: {
        uint8_t const_idx = chunk_read_byte(code, ip);
        assert(const_idx < code->const_table.size);
        vm_stack_push(vm, code->const_table.data[const_idx]);
        continue;
      }
      case OP_GET_FP: {
        uint8_t idx = chunk_read_byte(code, ip);
        vm_stack_push(vm, vm_from_frame_pointer(vm, idx));
        continue;
      }
      case OP_GET_UPVALUE: {
        uint8_t idx = chunk_read_byte(code, ip);
        assert(idx < lisp_closure_n_captures(frame->func));
        vm_stack_push(vm, lisp_closure_get_capture(frame->func, idx));
        continue;
      }
      case OP_GET_GLOBAL: {
        struct lisp_val top_value = vm_stack_pop(vm);
        struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, top_value);
        if (sym == NULL) {
          vm_raise_format_exception(vm, "cannot lookup value of type: %s",
                                    lisp_val_type_name(top_value));
          goto HANDLE_EXCEPTION;
        }
        const struct lisp_env_binding *binding =
            lisp_env_get(vm_global_env(vm), sym);
        if (binding == NULL) {
          vm_raise_format_exception(vm, "symbol not bound: %s",
                                    lisp_symbol_name(sym));
          goto HANDLE_EXCEPTION;
        }

        vm_stack_push(vm, binding->value);
        continue;
      }
      case OP_SET_GLOBAL: {
        struct lisp_val top_value = vm_stack_pop(vm);
        struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, top_value);
        if (sym == NULL) {
          vm_raise_format_exception(vm, "cannot bind value of type: %s",
                                    lisp_val_type_name(top_value));
          goto HANDLE_EXCEPTION;
        }

        struct lisp_val bind_value = vm_stack_pop(vm);
        lisp_env_set(vm_global_env(vm), sym, bind_value);
        continue;
      }
      case OP_POP:
        vm_stack_pop(vm);
        continue;
      case OP_CLEAR:
        vm_stack_frame_skip_clear(vm, 0);
        continue;
      case OP_SKIP_CLEAR: {
        uint8_t new_size = chunk_read_byte(code, ip);
        vm_stack_frame_skip_clear(vm, new_size);
        continue;
      }
      case OP_CALL: {
        uint8_t arg_count = chunk_read_byte(code, ip);
        struct lisp_val func_val = vm_stack_pop(vm);
        struct lisp_closure *func = check_call(vm, func_val, arg_count);
        if (func == NULL) {
          goto HANDLE_EXCEPTION;
        }
        vm_create_stack_frame(vm, func, arg_count);
        continue;
      }
      case OP_TAIL_CALL: {
        struct lisp_val func_val = vm_stack_pop(vm);
        unsigned arg_count = vm_stack_size(vm);
        struct lisp_closure *func = check_call(vm, func_val, arg_count);
        if (func == NULL) {
          goto HANDLE_EXCEPTION;
        }
        vm_replace_stack_frame(vm, func);
        continue;
      }
      case OP_RETURN:
        vm_stack_frame_return(vm);
        if (vm_current_frame_index(vm) < initial_frame) {
          // Nothing left to evaluate
          return EV_SUCCESS;
        } else {
          continue;
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
          goto HANDLE_EXCEPTION;
        }

        struct lisp_closure *cl =
            lisp_closure_create(lisp_val_as_obj(code_val), n_captures);
        vm_stack_push(vm, lisp_val_from_obj(cl));
        continue;
      }
      case OP_INIT_CLOSURE: {
        uint8_t n_captures = chunk_read_byte(code, ip);
        assert(vm_stack_size(vm) >= (unsigned)(n_captures + 1));
        struct lisp_val after_captures = vm_from_stack_pointer(vm, n_captures);
        struct lisp_closure *closure =
            lisp_val_cast(LISP_CLOSURE, after_captures);
        if (closure == NULL) {
          vm_raise_format_exception(
              vm, "cannot initialize object of type %s as closure",
              lisp_val_type_name(after_captures));
          goto HANDLE_EXCEPTION;
        }

        if (lisp_closure_n_captures(closure) != n_captures) {
          vm_raise_format_exception(vm,
                                    "tried to initialize closure with wrong "
                                    "number of captures. Expected %u, got %u",
                                    lisp_closure_n_captures(closure),
                                    n_captures);
          goto HANDLE_EXCEPTION;
        }

        for (int i = n_captures - 1; i >= 0; i--) {
          lisp_closure_set_capture(closure, i, vm_stack_pop(vm));
        }
        // Leave closure on top of the stack as if returned
        continue;
      }
      case OP_BUILD_REST_ARGS: {
        uint8_t start_fp = chunk_read_byte(code, ip);
        enum eval_status res = build_rest_args(vm, start_fp);
        if (res == EV_EXCEPTION) {
          goto HANDLE_EXCEPTION;
        }
        continue;
      }
      case OP_BRANCH: {
        int16_t offset = chunk_read_short(code, ip);
        int new_ip = *ip + offset - 2;
        if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
          vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                    new_ip);
          goto HANDLE_EXCEPTION;
        }
        *ip = new_ip;
        continue;
      }
      case OP_BRANCH_IF_FALSE: {
        int16_t offset = chunk_read_short(code, ip);
        if (lisp_val_is_false(vm_stack_pop(vm))) {
          int new_ip = *ip + offset - 2;
          if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
            vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                      new_ip);
            goto HANDLE_EXCEPTION;
          }
          *ip = new_ip;
        }
        continue;
      }
      case OP_INTRINSIC: {
        uint8_t index = chunk_read_byte(code, ip);
        enum eval_status res = call_intrinsic(index, vm);
        if (res == EV_EXCEPTION) {
          goto HANDLE_EXCEPTION;
        }
        continue;
      }
      case OP_SET_EX_HANDLER: {
        int16_t offset = chunk_read_short(code, ip);
        int handler_ip = *ip + offset - 2;
        if (handler_ip < 0 || code->bytecode.size <= (unsigned)handler_ip) {
          vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                    handler_ip);
          goto HANDLE_EXCEPTION;
        }
        vm_set_exception_handler(vm, handler_ip);
        continue;
      }
    }

    // Unhandled opcode (other instructions should continue to the top of the
    // loop)
    vm_raise_format_exception(vm, "unknown opcode: %d", op);
    // And then handle the error...

  HANDLE_EXCEPTION : {
    while (vm_current_frame_index(vm) >= initial_frame &&
           !vm_has_exception_handler(vm)) {
      vm_stack_frame_unwind(vm);
    }

    if (vm_has_exception_handler(vm)) {
      vm_run_exception_handler(vm);
      // Then continue on executing
    } else {
      // No exception handler (at least for the current execution sequence)
      return EV_EXCEPTION;
    }
  }
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
    struct lisp_cons *args_cons = lisp_val_cast(LISP_CONS, args);
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
