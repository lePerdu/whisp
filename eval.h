#ifndef EVAL_H_
#define EVAL_H_

#include "types.h"
#include "vm.h"

void init_global_eval_state(void);

enum eval_status eval(struct lisp_vm *vm, struct lisp_val ast);
enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func,
                            struct lisp_val args);

/**
 * Apply eval and handle exceptions by unwinding the stack.
 * To be used in top-level contexts, like the REPL or processing a file.
 */
enum eval_status eval_handle_exception(struct lisp_vm *vm, struct lisp_val ast);

#endif
