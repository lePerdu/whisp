#include "eval.h"

#include <stdbool.h>
#include <stdint.h>

#include "bytecode.h"
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

static enum eval_status check_call(struct lisp_vm *vm, struct lisp_val func_obj,
                                   unsigned arg_count, struct lisp_env **env,
                                   struct code_chunk **code) {
  struct lisp_closure *new_func = lisp_val_cast(LISP_CLOSURE, func_obj);
  if (new_func == NULL) {
    vm_raise_format_exception(vm, "cannot call value of type: %s",
                              lisp_val_type(func_obj));
    return EV_EXCEPTION;
  }

  unsigned req_args = lisp_closure_arg_count(new_func);
  if (arg_count < req_args) {
    vm_raise_format_exception(vm, "not enough arguments");
    return EV_EXCEPTION;
  }
  if (arg_count > req_args && !lisp_closure_is_variadic(new_func)) {
    vm_raise_format_exception(vm, "too many arguments");
    return EV_EXCEPTION;
  }

  gc_push_root(func_obj);
  // Create a new execution environment
  *env = lisp_env_create(lisp_closure_env(new_func));
  *code = lisp_closure_code(new_func);
  gc_pop_root_expect(func_obj);

  return EV_SUCCESS;
}

/**
 * Evaluate bytecode until the call stack is empty.
 */
static enum eval_status eval_bytecode(struct lisp_vm *vm) {
  const unsigned initial_frame = vm_current_frame_index(vm);

  while (true) {
    struct stack_frame *frame = vm_current_frame(vm);
    struct code_chunk *code = frame->code;
    unsigned *ip = &frame->instr_pointer;
    assert(*ip < code->bytecode.size);

    enum bytecode_op op = chunk_read_byte(code, ip);

    // TODO Error checking before the asserts (including in VM functions)
    switch (op) {
      case OP_CONST: {
        uint8_t const_idx = chunk_read_byte(code, ip);
        assert(const_idx < code->const_table.size);
        vm_stack_push(vm, code->const_table.data[const_idx]);
        break;
      }
      case OP_LOOKUP: {
        struct lisp_val top_value = vm_stack_pop(vm);
        struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, top_value);
        if (sym == NULL) {
          vm_raise_format_exception(vm, "cannot lookup value of type: %s",
                                    lisp_val_type_name(top_value));
          return EV_EXCEPTION;
        }
        const struct lisp_env_binding *binding = lisp_env_get(frame->env, sym);
        if (binding == NULL) {
          vm_raise_format_exception(vm, "symbol not bound: %s",
                                    lisp_symbol_name(sym));
          return EV_EXCEPTION;
        }

        vm_stack_push(vm, binding->value);
        break;
      }
      case OP_BIND: {
        struct lisp_val top_value = vm_stack_pop(vm);
        struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, top_value);
        if (sym == NULL) {
          vm_raise_format_exception(vm, "cannot bind value of type: %s",
                                    lisp_val_type_name(top_value));
          return EV_EXCEPTION;
        }

        struct lisp_val bind_value = vm_stack_pop(vm);
        lisp_env_set(frame->env, sym, bind_value);
        break;
      }
      case OP_BIND_GLOBAL: {
        struct lisp_val top_value = vm_stack_pop(vm);
        struct lisp_symbol *sym = lisp_val_cast(LISP_SYMBOL, top_value);
        if (sym == NULL) {
          vm_raise_format_exception(vm, "cannot bind value of type: %s",
                                    lisp_val_type_name(top_value));
          return EV_EXCEPTION;
        }

        struct lisp_val bind_value = vm_stack_pop(vm);
        lisp_env_set(vm_global_env(vm), sym, bind_value);
        break;
      }
      case OP_POP:
        vm_stack_pop(vm);
        break;
      case OP_DUP:
        vm_stack_push(vm, vm_stack_top(vm));
        break;
      case OP_DUP_FP: {
        uint8_t idx = chunk_read_byte(code, ip);
        vm_stack_push(vm, vm_from_frame_pointer(vm, idx));
        break;
      }
      case OP_CLEAR:
        vm_stack_frame_skip_clear(vm, 0);
        break;
      case OP_SKIP_CLEAR: {
        uint8_t new_size = chunk_read_byte(code, ip);
        vm_stack_frame_skip_clear(vm, new_size);
        break;
      }
      case OP_CALL: {
        uint8_t arg_count = chunk_read_byte(code, ip);
        struct lisp_val func = vm_stack_pop(vm);
        struct lisp_env *new_env;
        struct code_chunk *new_code;
        if (check_call(vm, func, arg_count, &new_env, &new_code) ==
            EV_EXCEPTION) {
          return EV_EXCEPTION;
        }
        vm_create_stack_frame(vm, new_env, new_code, arg_count);
        break;
      }
      case OP_TAIL_CALL: {
        struct lisp_val func = vm_stack_pop(vm);
        unsigned arg_count = vm_stack_size(vm);
        struct lisp_env *new_env;
        struct code_chunk *new_code;
        if (check_call(vm, func, arg_count, &new_env, &new_code) ==
            EV_EXCEPTION) {
          return EV_EXCEPTION;
        }
        vm_replace_stack_frame(vm, new_env, new_code);
        break;
      }
      case OP_RETURN:
        vm_stack_frame_return(vm);
        if (vm_current_frame_index(vm) < initial_frame) {
          // Nothing left to evaluate
          return EV_SUCCESS;
        } else {
          break;
        }
      case OP_MAKE_CLOSURE: {
        uint8_t req_arg_count = chunk_read_byte(code, ip);
        uint8_t is_variadic = chunk_read_byte(code, ip);
        struct lisp_val top_value = vm_stack_pop(vm);
        if (!is_chunk(top_value)) {
          vm_raise_format_exception(
              vm,
              "ca(((fn () (fn x 1))))nnot make closure with code of type: %s",
              lisp_val_type_name(top_value));
          return EV_EXCEPTION;
        }

        struct lisp_closure *cl = lisp_closure_create(
            req_arg_count, is_variadic, frame->env, lisp_val_as_obj(top_value));
        vm_stack_push(vm, lisp_val_from_obj(cl));
        break;
      }
      case OP_BUILD_REST_ARGS: {
        uint8_t start_fp = chunk_read_byte(code, ip);
        enum eval_status res = build_rest_args(vm, start_fp);
        if (res == EV_EXCEPTION) {
          return EV_EXCEPTION;
        }
        break;
      }
      case OP_BRANCH: {
        int16_t offset = chunk_read_short(code, ip);
        int new_ip = *ip + offset - 2;
        if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
          vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                    new_ip);
          return EV_EXCEPTION;
        }
        *ip = new_ip;
        break;
      }
      case OP_BRANCH_IF_FALSE: {
        int16_t offset = chunk_read_short(code, ip);
        if (lisp_val_is_false(vm_stack_pop(vm))) {
          int new_ip = *ip + offset - 2;
          if (new_ip < 0 || code->bytecode.size <= (unsigned)new_ip) {
            vm_raise_format_exception(vm, "branch target out of bounds: %d",
                                      new_ip);
            return EV_EXCEPTION;
          }
          *ip = new_ip;
        }
        continue;
      }
      default:
        vm_raise_format_exception(vm, "unknown opcode: %d", op);
        return EV_EXCEPTION;
    }
  }
}

/**
 * Evalute bytecode, unwinding the stack on error.
 */
enum eval_status do_eval(struct lisp_vm *vm) {
  enum eval_status res = eval_bytecode(vm);
  if (res == EV_EXCEPTION) {
    vm_stack_frame_unwind_all(vm);
  }
  return res;
}

enum eval_status eval_closure(struct lisp_vm *vm, struct lisp_closure *cl) {
  struct lisp_env *new_env;
  struct code_chunk *new_code;
  // TODO Should this always create a new environment? It's not necessary for
  // top-level compiled closures, but makes the behavior consistent between
  // top-level REPL and calling `(eval)`.
  enum eval_status res =
      check_call(vm, lisp_val_from_obj(cl), 0, &new_env, &new_code);
  if (res == EV_EXCEPTION) {
    return res;
  }

  vm_create_stack_frame(vm, new_env, new_code, 0);
  return do_eval(vm);
}

enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func,
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

  struct lisp_env *new_env;
  struct code_chunk *new_code;
  enum eval_status res = check_call(vm, func, arg_count, &new_env, &new_code);
  if (res == EV_EXCEPTION) {
    return res;
  }

  vm_create_stack_frame(vm, new_env, new_code, arg_count);
  return do_eval(vm);
}
