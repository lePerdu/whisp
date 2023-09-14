#ifndef VM_H_
#define VM_H_

#include <stdbool.h>

#include "types.h"

struct lisp_vm;

struct lisp_vm *vm_create(void);

/**
 * Return whether there is an active exception.
 * This is needed since currently NIL can be raised as an exception.
 * TODO Collapse all of these into a single vm_current_exception() which returns
 * a pointer to the exception (returning NULL if there is none and allowing it
 * to be overwritten).
 */
bool vm_has_exception(const struct lisp_vm *vm);
struct lisp_val vm_current_exception(const struct lisp_vm *vm);
void vm_raise_exception(struct lisp_vm *vm, struct lisp_val exception);
void vm_clear_exception(struct lisp_vm *vm);

void vm_raise_format_exception(struct lisp_vm *vm, const char *format, ...);

#define vm_raise_func_exception(vm, message, ...) \
  vm_raise_format_exception((vm), "%s: " message, \
                            __func__ __VA_OPT__(, ) __VA_ARGS__)

/** Returns whether the current stack frame is empty. */
bool vm_stack_empty(const struct lisp_vm *vm);

/** Push another value to the current stack frame. */
void vm_stack_push(struct lisp_vm *vm, struct lisp_val v);

struct lisp_val vm_stack_top(struct lisp_vm *vm);

/**
 * Pop a value from the current stack frame.
 * The stack frame must not be empty.
 */
struct lisp_val vm_stack_pop(struct lisp_vm *vm);

/**
 * Delete all items from the current stack frame.
 */
void vm_stack_frame_clear(struct lisp_vm *vm);

struct lisp_env *vm_current_env(struct lisp_vm *vm);

/**
 * Create a new stack frame with a given execution environment.
 */
void vm_create_stack_frame(struct lisp_vm *vm, struct lisp_env *func_env,
                           unsigned arg_count);

/**
 * Replace the current stack frame with a new one to initiate a tail call.
 */
void vm_create_tail_stack_frame(struct lisp_vm *vm, struct lisp_env *func_env,
                                unsigned arg_count);

/**
 * Return from the current frame. The return value is on the top of the
 * stack.
 */
void vm_stack_frame_return(struct lisp_vm *vm);

struct stack_frame_state {
  unsigned frame_index;
  unsigned stack_size;
};

void vm_stack_frame_save(const struct lisp_vm *vm,
                         struct stack_frame_state *state);
void vm_stack_frame_unwind_to(struct lisp_vm *vm,
                              const struct stack_frame_state *state);

struct lisp_val vm_from_frame_pointer(const struct lisp_vm *vm, unsigned index);
struct lisp_val vm_from_stack_pointer(const struct lisp_vm *vm,
                                      unsigned rev_index);

#endif