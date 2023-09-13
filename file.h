#ifndef FILE_H_
#define FILE_H_

#include "vm.h"

/**
 * Read contents of a file.
 *
 * On failure, returns NULL and sets current_exception to the error message.
 */
struct lisp_string *read_file(struct lisp_vm *vm, const char *filename);

/**
 * Read and run a file.
 */
enum eval_status load_file(struct lisp_vm *vm, const char *filename);

#endif
