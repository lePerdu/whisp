#ifndef FILE_H_
#define FILE_H_

/**
 * Read contents of a file.
 *
 * On failure, returns NULL and sets current_exception to the error message.
 */
struct lisp_string *read_file(const char *filename);

/**
 * Read and run a file.
 */
enum eval_status load_file(const char *filename);

#endif
