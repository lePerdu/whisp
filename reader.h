#ifndef READER_H_
#define READER_H_

#include "types.h"

enum parse_status {
  P_SUCCESS = 0,
  P_EMPTY,
  P_FAILED,
};

struct source_pos {
  /** 0-indexed line number. */
  unsigned line;
  /** 0-indexed column. */
  unsigned col;
};

struct parse_error {
  const char *filename;
  struct source_pos error_pos;
};

struct parse_res {
  enum parse_status status;
  struct parse_error error;
};

struct lisp_string *parse_error_format(const struct parse_error *error);

/**
 * Parse input into a single AST.
 */
struct parse_res read_str(const char *filename, const char *input,
                          struct lisp_val *output);

/**
 * Like read_str, but read multiple top-level ASTs into a list.
 */
struct parse_res read_str_many(const char *filename, const char *input,
                               struct lisp_val *output);

bool is_valid_symbol(const char *name);

#endif
