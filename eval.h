#ifndef EVAL_H_
#define EVAL_H_

#include "bytecode.h"
#include "types.h"
#include "vm.h"

/**
 * Possible evaluation conditions.
 */
enum eval_status {
  EV_SUCCESS = 0,
  EV_EXCEPTION,
};

/**
 * Evaluate bytecode in a code chunk at the top level.
 */
enum eval_status eval_chunk(struct lisp_vm *vm, struct code_chunk *code);

/**
 * Apply a function to a list of arguments at the top-level.
 */
enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func,
                            struct lisp_val args);

#endif
