#ifndef CORE_H_
#define CORE_H_

#include <stdint.h>

#include "eval.h"
#include "types.h"

enum eval_status call_intrinsic(uint8_t index, struct lisp_vm *vm);
void define_builtins(struct lisp_env *global_env);

#endif
