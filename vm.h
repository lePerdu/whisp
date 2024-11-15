#ifndef VM_H_
#define VM_H_

#include <stdbool.h>
#include <time.h>

#include "types.h"
#include "val_array.h"

/**
 * Stack frame for an executing function.
 */
struct stack_frame {
  /** Index into the stack */
  unsigned frame_pointer;
  struct lisp_closure *func;
  /** Offset into the bytecode array of the function. */
  unsigned instr_pointer;
  /**
   * Dynamic state of the current stack frame.
   *
   * By default, this is the same as the parent stack frame, but is modified
   * with `OP_SET_DYNAMIC_STATE`.
   */
  struct lisp_val dynamic_state;
};

struct call_stack {
  size_t size;
  size_t cap;
  struct stack_frame *data;
};

struct lisp_vm {
  struct lisp_obj header;

  // TODO Store global env as a base call frame?
  struct lisp_env *global_env;
  struct lisp_val global_dynamic_state;
  struct call_stack call_frames;
  struct val_array stack;

  struct lisp_val primitive_error_handler;
  bool is_fatal_error;
  struct lisp_val current_exception;
};

struct lisp_vm *vm_create(struct lisp_env *global_env,
                          struct lisp_val dynamic_state);

static inline struct lisp_val vm_current_exception(const struct lisp_vm *vm) {
  return vm->current_exception;
}

static inline void vm_raise_exception(struct lisp_vm *vm,
                                      struct lisp_val exception) {
  vm->is_fatal_error = false;
  vm->current_exception = exception;
}

static inline void vm_clear_exception(struct lisp_vm *vm) {
  vm->is_fatal_error = false;
  vm->current_exception = LISP_VAL_NIL;
}

void vm_raise_format_exception(struct lisp_vm *vm, const char *format, ...);

#define vm_raise_func_exception(vm, message, ...) \
  vm_raise_format_exception((vm), "%s: " message, \
                            __func__ __VA_OPT__(, ) __VA_ARGS__)

/**
 * Returns the size of the current stack frame.
 * When a function is first called, this will be the number of arguments passed.
 */
unsigned vm_stack_size(const struct lisp_vm *vm);

/** Push another value to the current stack frame. */
void vm_stack_push(struct lisp_vm *vm, struct lisp_val v);

struct lisp_val vm_stack_top(struct lisp_vm *vm);

/**
 * Pop a value from the current stack frame.
 * The stack frame must not be empty.
 */
struct lisp_val vm_stack_pop(struct lisp_vm *vm);

/**
 * Delete `delete_n` items after the top `skip_n`.
 */
void vm_stack_frame_skip_delete(struct lisp_vm *vm, unsigned skip_n,
                                unsigned delete_n);

static inline unsigned vm_current_frame_index(const struct lisp_vm *vm) {
  return vm->call_frames.size;
}

struct stack_frame *vm_current_frame(struct lisp_vm *vm);
struct stack_frame *vm_parent_frame(struct lisp_vm *vm);

/**
 * Create a new stack frame with a given execution environment.
 */
void vm_create_stack_frame(struct lisp_vm *vm, struct lisp_closure *func,
                           unsigned arg_count);

/**
 * Replace the current stack frame with a new one to initiate a tail call.
 */
void vm_replace_stack_frame(struct lisp_vm *vm, struct lisp_closure *func);

/**
 * Return from the current frame. The return value is on the top of the
 * stack.
 */
void vm_stack_frame_return(struct lisp_vm *vm);

/**
 * Return from a specified frame.
 */
void vm_stack_frame_return_from(struct lisp_vm *vm, unsigned frame_index);

static inline struct lisp_val vm_get_dynamic_state(struct lisp_vm *vm) {
  struct stack_frame *cur = vm_current_frame(vm);
  return cur != NULL ? cur->dynamic_state : vm->global_dynamic_state;
}

/**
 * Jump to a set exception handler and setup for running it:
 * - Push the exception on the stack (TODO Also restore the stack to
 * pre-exception state)
 * - Clear the exception handler
 * - Clear the current VM exception (the handler can re-raise if it wants)
 */
void vm_run_exception_handler(struct lisp_vm *vm);

void vm_stack_frame_unwind(struct lisp_vm *vm);

struct lisp_val vm_from_frame_pointer(const struct lisp_vm *vm, unsigned index);
struct lisp_val vm_from_stack_pointer(const struct lisp_vm *vm,
                                      unsigned rev_index);

#endif
