#ifndef EVAL_H_
#define EVAL_H_

#include "types.h"
#include "vm.h"

void init_global_eval_state(void);

enum eval_status eval(struct lisp_vm *vm, struct lisp_val ast);
enum eval_status eval_apply(struct lisp_vm *vm, struct lisp_val func,
                            struct lisp_val args);

#endif
