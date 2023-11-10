#ifndef FILE_H_
#define FILE_H_

#include <stdio.h>

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
struct lisp_closure *compile_file(struct lisp_vm *vm,
                                  struct lisp_string *filename);

/**
 * Compile contents of a string into an executable closure.
 */
struct lisp_closure *compile_string(struct lisp_vm *vm,
                                    struct lisp_string *filename,
                                    struct lisp_string *contents);

/**
 * Read a single line from a file stream.
 */
struct lisp_string *read_line(FILE *stream);

/**
 * Simple existence check to be provided as a builtin.
 */
bool file_exists(const char *filename);

/**
 * Delete a file. Returns true on success, false on error.
 */
bool delete_file(const char *filename);

struct lisp_string *get_current_directory(void);

#endif
