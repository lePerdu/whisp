#ifndef COMPILER_H_
#define COMPILER_H_

#include <stdbool.h>

#include "eval.h"
#include "memory.h"
#include "reader.h"
#include "types.h"
#include "vm.h"

void init_global_compile_state(void);

/**
 * Compile a top-level expression. Returns a compiled code chunk on success,
 * NULL on error. In case of error, the exception is raised in the passed-in VM.
 */
struct lisp_closure *compile_top_level(struct lisp_vm *vm,
                                       struct parse_output *metadata,
                                       struct lisp_val ast);

/**
 * Compile and then evaluate a top-level expression.
 */
enum eval_status compile_eval(struct lisp_vm *vm, struct parse_output *metadata,
                              struct lisp_val ast);

extern struct lisp_symbol *SYMBOL_DO;
extern struct lisp_symbol *SYMBOL_IF;
extern struct lisp_symbol *SYMBOL_DEF;
extern struct lisp_symbol *SYMBOL_DEFSYNTAX;
extern struct lisp_symbol *SYMBOL_FN;
extern struct lisp_symbol *SYMBOL_LET;
extern struct lisp_symbol *SYMBOL_QUOTE;

#endif
