#ifndef READER_H_
#define READER_H_

#include "types.h"

enum parse_status {
  P_SUCCESS = 0,
  P_EMPTY,
  P_ERROR,
};

struct source_pos {
  /** 0-indexed line number. */
  unsigned line;
  /** 0-indexed column. */
  unsigned col;
};

/**
 * Bundle of all parser output:
 * - Parsed datum
 * - Source location metadata
 * - Errors (if any)
 */
struct parse_output {
  struct lisp_obj header;
  enum parse_status status;
  struct lisp_string *filename;
  struct lisp_hash_table *source_map;

  struct lisp_val datum;

  struct {
    struct lisp_string *message;
    struct source_pos pos;
  } error;
};

struct parse_output *parse_output_create_simple(struct lisp_string *filename,
                                                struct lisp_val ast);
const struct source_pos *parse_output_get_source_pos(struct parse_output *p,
                                                     struct lisp_val datum);

/**
 * Return the filename, or a placeholder string if there is no associated file
 * name.
 */
const char *parse_output_filename(struct parse_output *p);
struct lisp_string *parse_error_format(struct parse_output *error);

/**
 * Parse input into a single AST.
 */
struct parse_output *read_str(struct lisp_string *filename, const char *input);

/**
 * Like read_str, but read multiple top-level ASTs into a list.
 */
struct parse_output *read_str_many(struct lisp_string *filename,
                                   const char *input);

bool is_valid_symbol(const char *name);

/**
 * Try to parse text as an integer.
 *
 * Return the parsed value, or NIL on error.
 */
struct lisp_val read_int(const char *input);

/**
 * Try to parse text as a real number.
 *
 * Return the parsed value, or NIL on error.
 */
struct lisp_val read_real(const char *input);

#endif
