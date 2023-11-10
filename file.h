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
 * Compile the contents of a file into an executable closure.
 */
struct lisp_closure *compile_file(struct lisp_vm *vm, const char *filename);

/**
 * Simple existence check to be provided as a builtin.
 */
bool file_exists(const char *filename);

/**
 * Delete a file. Returns true on success, false on error.
 */
bool delete_file(const char *filename);

#endif
