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
 * Delete all items from the current stack frame.
 */
void vm_stack_frame_skip_clear(struct lisp_vm *vm, unsigned n);

/**
 * Stack frame for an executing function.
 */
struct stack_frame {
  /** Index into the stack */
  unsigned frame_pointer;
  struct lisp_env *env;
  struct code_chunk *code;
  /** Offset into the bytecode array of the function. */
  unsigned instr_pointer;
};

unsigned vm_current_frame_index(const struct lisp_vm *vm);
struct stack_frame *vm_current_frame(struct lisp_vm *vm);
struct lisp_env *vm_current_env(struct lisp_vm *vm);
struct lisp_env *vm_global_env(struct lisp_vm *vm);

/**
 * Create a new stack frame with a given execution environment.
 */
void vm_create_stack_frame(struct lisp_vm *vm, struct lisp_env *env,
                           struct code_chunk *code, unsigned arg_count);

/**
 * Replace the current stack frame with a new one to initiate a tail call.
 */
void vm_replace_stack_frame(struct lisp_vm *vm, struct lisp_env *env,
                            struct code_chunk *code);

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
void vm_stack_frame_unwind_all(struct lisp_vm *vm);

struct lisp_val vm_from_frame_pointer(const struct lisp_vm *vm, unsigned index);
struct lisp_val vm_from_stack_pointer(const struct lisp_vm *vm,
                                      unsigned rev_index);

#endif
