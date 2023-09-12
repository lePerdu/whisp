#ifndef READER_H_
#define READER_H_

#include "types.h"

enum parse_res {
  P_SUCCESS = 0,
  P_EMPTY,
  P_FAILED,
};

/**
 * Parse input into a single AST.
 */
enum parse_res read_str(const char *input, struct lisp_val *output);

/**
 * Like read_str, but read multiple top-level ASTs into a list.
 */
enum parse_res read_str_many(const char *input, struct lisp_val *output);

bool is_valid_symbol(const char *name);

#endif
