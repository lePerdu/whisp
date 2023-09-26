#ifndef COMPILER_H_
#define COMPILER_H_

#include <stdbool.h>

#include "eval.h"
#include "memory.h"
#include "types.h"
#include "vm.h"

void init_global_compile_state(void);

/**
 * Compile a top-level expression. Returns a compiled code chunk on success,
 * NULL on error. In case of error, the exception is raised in the passed-in VM.
 */
struct code_chunk *compile_top_level(struct lisp_vm *vm, struct lisp_val ast);

/**
 * Compile and then evaluate a top-level expression.
 */
enum eval_status compile_eval(struct lisp_vm *vm, struct lisp_val ast);

#endif
