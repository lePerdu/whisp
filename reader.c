#include "reader.h"

#include <ctype.h>
#include <printf.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
#include "memory.h"
#include "types.h"

enum token_type {
  TOK_EOF = -1,
  TOK_INVALID = 0,
  TOK_INT,
  TOK_REAL,
  TOK_CHAR,
  TOK_SYMBOL,
  // Special token that can't be re-defined
  // TODO Treat this some other way?
  TOK_NIL,
  TOK_TILDE_AT,

  TOK_STRING = '"',
  TOK_LPAREN = '(',
  TOK_RPAREN = ')',
  TOK_DOT = '.',
  TOK_QUOTE = '\'',
  TOK_BACKTICK = '`',
  TOK_TILDE = '~',
  TOK_AT = '@',
};

#define PRIM_PREFIX '#'
#define PRIM_CHAR_PREFIX '\\'
#define ESCAPE_CHAR '\\'
#define COMMENT_CHAR ';'

struct slice {
  size_t size;
  const char *data;
};

struct token {
  enum token_type type;
  union {
    long as_integer;
    double as_real;
    char as_char;
    struct slice as_slice;
  };
};

struct reader {
  const char *input;
  /** Offset in the input string. */
  size_t pos;
  /** Human-readable source location. */
  struct source_pos source_pos;
  struct token token;
};

static void reader_init(struct reader *r, const char *input) {
  r->input = input;
  r->pos = 0;
  r->source_pos.line = 0;
  r->source_pos.col = 0;
  r->token.type = TOK_INVALID;
}

static char reader_peek(const struct reader *r) { return r->input[r->pos]; }

static void reader_advance(struct reader *r) {
  char cur = reader_peek(r);
  if (cur == 0) {
    return;
  }

  if (cur == '\n') {
    r->source_pos.line++;
    r->source_pos.col = 0;
  } else {
    r->source_pos.col++;
  }
  r->pos++;
}

static void reader_advance_n(struct reader *r, unsigned n) {
  for (; n > 0; n--) {
    reader_advance(r);
  }
}

static bool is_lisp_space(char c) { return isspace(c) || c == ','; }

static bool is_atom_char(char c) {
  switch (c) {
    case 0:
    case TOK_LPAREN:
    case TOK_RPAREN:
    case TOK_STRING:
    case TOK_QUOTE:
    case TOK_BACKTICK:
    case COMMENT_CHAR:
      return false;
  }
  return !is_lisp_space(c);
}

static void skip_white(struct reader *r) {
  while (is_lisp_space(reader_peek(r))) {
    reader_advance(r);
  }
}

static bool is_eol(char c) { return c == '\n' || c == 0; }

static void skip_line(struct reader *r) {
  while (!is_eol(reader_peek(r))) {
    reader_advance(r);
  }
}

static enum parse_status parse_int(struct reader *r, const char *atom_start,
                                   const char *atom_end) {
  char *endptr;
  long n = strtol(atom_start, &endptr, 10);
  if (endptr != atom_end) {
    return P_FAILED;
  }

  r->token = (struct token){.type = TOK_INT, .as_integer = n};
  return P_SUCCESS;
}

static enum parse_status parse_real(struct reader *r, const char *atom_start,
                                    const char *atom_end) {
  char *endptr;
  // TODO strtod has some undesired behavior, like parsing as hex and parsing
  // nan/infinity.
  double x = strtod(atom_start, &endptr);
  if (endptr != atom_end) {
    return P_FAILED;
  }

  r->token = (struct token){.type = TOK_REAL, .as_real = x};
  return P_SUCCESS;
}

static bool equals_literal(const char *str_start, size_t str_len,
                           const char *literal, size_t literal_len) {
  assert(strlen(literal) == literal_len);
  return str_len == literal_len && memcmp(str_start, literal, str_len) == 0;
}

static enum parse_status parse_char(struct reader *r, const char *atom_start,
                                    const char *atom_end) {
  if (atom_start[0] == PRIM_PREFIX && atom_start[1] == PRIM_CHAR_PREFIX) {
    atom_start += 2;

    r->token.type = TOK_CHAR;
    size_t atom_len = atom_end - atom_start;
    if (atom_len == 0) {
      // TODO Space character like in scheme?
      return P_FAILED;
    } else if (atom_len == 1) {
      r->token.as_char = *atom_start;
    } else if (equals_literal(atom_start, atom_len, "null", 4)) {
      r->token.as_char = 0;
    } else if (equals_literal(atom_start, atom_len, "space", 5)) {
      r->token.as_char = ' ';
    } else if (equals_literal(atom_start, atom_len, "tab", 3)) {
      r->token.as_char = '\t';
    } else if (equals_literal(atom_start, atom_len, "newline", 7)) {
      r->token.as_char = '\n';
    } else {
      return P_FAILED;
    }
    return P_SUCCESS;
  } else {
    return P_FAILED;
  }
}

/**
 * Parse symbols and special constants.
 */
static enum parse_status parse_symbol(struct reader *r, const char *atom_start,
                                      const char *atom_end) {
  size_t atom_len = atom_end - atom_start;

  if (atom_len == 3 && memcmp(atom_start, "nil", 3) == 0) {
    r->token.type = TOK_NIL;
  } else {
    r->token =
        (struct token){.type = TOK_SYMBOL, .as_slice = {atom_len, atom_start}};
  }
  // Always passes for now
  return P_SUCCESS;
}

static enum parse_status read_next_atom(struct reader *r) {
  const char *atom_start = &r->input[r->pos];
  const char *atom_end = atom_start;
  while (is_atom_char(*atom_end)) {
    atom_end++;
  }
  if (atom_start == atom_end) {
    return P_FAILED;
  }

  enum parse_status res = parse_int(r, atom_start, atom_end);
  if (res == P_SUCCESS) {
    goto SUCCESS;
  }

  res = parse_real(r, atom_start, atom_end);
  if (res == P_SUCCESS) {
    goto SUCCESS;
  }

  res = parse_char(r, atom_start, atom_end);
  if (res == P_SUCCESS) {
    goto SUCCESS;
  }

  res = parse_symbol(r, atom_start, atom_end);
  if (res == P_SUCCESS) {
    goto SUCCESS;
  }

  return P_FAILED;

SUCCESS:
  reader_advance_n(r, atom_end - atom_start);
  return P_SUCCESS;
}

static int translate_unescaped(char c) {
  switch (c) {
    case '0':
      return 0;
    case '"':
      return '"';
    case '\\':
      return '\\';
    case 'n':
      return '\n';
    case 't':
      return '\t';
    default:
      return -1;
  }
}

static bool is_str_end(char c) { return c == TOK_STRING || is_eol(c); }

static enum parse_status read_string_atom(struct reader *r) {
  const char *str_start = &r->input[r->pos];
  // Include starting quote
  unsigned str_tok_len = 1;
  char c;
  while (!is_str_end(c = str_start[str_tok_len])) {
    str_tok_len++;
    if (c == ESCAPE_CHAR) {
      char escape_char = str_start[str_tok_len];
      // Make sure it's a valid escape character
      if (translate_unescaped(escape_char) == -1) {
        log("invalid escape character: '%c'", escape_char);
        return P_FAILED;
      } else {
        str_tok_len++;
      }
    }
  }

  // Ended for any reason other than end quote
  if (c != TOK_STRING) {
    log("expected '\"', got '%c'", c);
    return P_FAILED;
  }

  // Include ending quote
  str_tok_len++;
  r->token = (struct token){
      .type = TOK_STRING,
      .as_slice = {.size = str_tok_len, .data = str_start},
  };

  reader_advance_n(r, str_tok_len);
  return P_SUCCESS;
}

static enum parse_status reader_next(struct reader *r) {
START:
  skip_white(r);

  char c = reader_peek(r);
  switch (c) {
    case 0:
      r->token.type = TOK_EOF;
      return P_SUCCESS;
    case COMMENT_CHAR:
      skip_line(r);
      goto START;
    case TOK_LPAREN:
    case TOK_RPAREN:
    case TOK_QUOTE:
    case TOK_BACKTICK:
    case TOK_AT:
      r->token.type = c;
      reader_advance(r);
      return P_SUCCESS;
    case TOK_TILDE:
      reader_advance(r);
      if (reader_peek(r) == TOK_AT) {
        r->token.type = TOK_TILDE_AT;
        reader_advance(r);
      } else {
        r->token.type = TOK_TILDE;
      }
      return P_SUCCESS;
    case TOK_DOT:
      // These are special by themselves, but not if part of an atom
      if (!is_atom_char(r->input[r->pos + 1])) {
        r->token.type = c;
        reader_advance(r);
        return P_SUCCESS;
      } else {
        break;
      }
    case TOK_STRING:
      return read_string_atom(r);
  }

  return read_next_atom(r);
}

static enum parse_status reader_expect(struct reader *r, enum token_type t) {
  enum parse_status res = reader_next(r);
  if (res != P_SUCCESS) {
    return res;
  }
  return r->token.type == t ? P_SUCCESS : P_FAILED;
}

static bool reader_at_eof(struct reader *r) { return reader_peek(r) == 0; }

static enum parse_status read_form(struct reader *r, struct lisp_val *output);

static enum parse_status read_list_or_pair(struct reader *r,
                                           struct lisp_val *output) {
  enum parse_status res;
  res = reader_next(r);
  if (res != P_SUCCESS) {
    return res;
  }

  struct list_builder builder;
  list_builder_init(&builder);

  while (r->token.type != TOK_RPAREN) {
    struct lisp_val next;
    res = read_form(r, &next);
    if (res != P_SUCCESS) {
      break;
    }
    list_builder_append(&builder, next);

    res = reader_next(r);
    if (res != P_SUCCESS) {
      return res;
    }

    if (r->token.type == TOK_DOT) {
      // Turn the node into a pair instead of a list

      // Skip the dot
      res = reader_next(r);
      if (res != P_SUCCESS) {
        break;
      }

      struct lisp_val last;
      res = read_form(r, &last);
      if (res != P_SUCCESS) {
        break;
      }
      list_builder_end_pair(&builder, last);

      res = reader_expect(r, TOK_RPAREN);
      break;
    }
  }

  // This is built and assigned even in case of failure, which is fine
  *output = list_build(&builder);
  return res;
}

static enum parse_status read_macro(struct reader *r, const char *name,
                                    struct lisp_val *output) {
  enum parse_status res;
  res = reader_next(r);
  if (res != P_SUCCESS) {
    return res;
  }

  struct lisp_val inner;
  res = read_form(r, &inner);
  if (res != P_SUCCESS) {
    return res;
  }

  gc_push_root(inner);

  // Use a builder to simplify GC root management
  struct list_builder builder;
  list_builder_init(&builder);

  list_builder_append(&builder,
                      lisp_val_from_obj(lisp_symbol_create_cstr(name)));
  list_builder_append(&builder, inner);
  *output = list_build(&builder);

  gc_pop_root_expect(inner);
  return P_SUCCESS;
}

static struct lisp_val build_lisp_string(struct slice raw_string) {
  struct str_builder builder;
  // TODO Count escape characters in tokenizer so this can be an exact size
  str_builder_init_cap(&builder, raw_string.size);
  // Quotes are included in the slice, so skip over them
  for (unsigned i = 1; i < raw_string.size - 1; i++) {
    char c = raw_string.data[i];
    if (c == ESCAPE_CHAR) {
      i++;
      c = raw_string.data[i];
      // Escape character is already checked in the tokenizer
      str_builder_append(&builder, translate_unescaped(c));
    } else {
      str_builder_append(&builder, c);
    }
  }
  return lisp_val_from_obj(str_build(&builder));
}

static enum parse_status read_form(struct reader *r, struct lisp_val *output) {
  switch (r->token.type) {
    case TOK_INT:
      *output = lisp_val_from_int(r->token.as_integer);
      return P_SUCCESS;
    case TOK_REAL:
      *output = lisp_val_from_real(r->token.as_real);
      return P_SUCCESS;
    case TOK_CHAR:
      *output = lisp_val_from_char(r->token.as_char);
      return P_SUCCESS;
    case TOK_SYMBOL: {
      struct slice s = r->token.as_slice;
      *output = lisp_val_from_obj(lisp_symbol_create(s.data, s.size));
      return P_SUCCESS;
    }
    case TOK_STRING:
      *output = build_lisp_string(r->token.as_slice);
      return P_SUCCESS;
    case TOK_NIL:
      *output = LISP_VAL_NIL;
      return P_SUCCESS;
    case TOK_LPAREN:
      return read_list_or_pair(r, output);
    case TOK_RPAREN:
      return P_FAILED;
    case TOK_QUOTE:
      return read_macro(r, "quote", output);
    case TOK_BACKTICK:
      return read_macro(r, "quasiquote", output);
    case TOK_TILDE:
      return read_macro(r, "unquote", output);
    case TOK_AT:
      return read_macro(r, "deref", output);
    case TOK_TILDE_AT:
      return read_macro(r, "splice-unquote", output);
    default:
      return P_FAILED;
  }
}

static inline struct parse_res make_error_res(const struct reader *r) {
  return (struct parse_res){
      .status = P_FAILED,
      .error = {.error_pos = r->source_pos},
  };
}

static inline struct parse_res make_status_res(enum parse_status status) {
  return (struct parse_res){
      .status = status,
  };
}

struct parse_res read_str(const char *input, struct lisp_val *output) {
  struct reader r;
  reader_init(&r, input);

  enum parse_status status;

  status = reader_next(&r);
  if (status == P_FAILED) {
    return make_error_res(&r);
  }

  if (r.token.type == TOK_EOF) {
    return make_status_res(P_EMPTY);
  }

  status = read_form(&r, output);
  if (status == P_FAILED) {
    return make_error_res(&r);
  }

  status = reader_expect(&r, TOK_EOF);
  if (status == P_FAILED) {
    return make_error_res(&r);
  }
  return make_status_res(P_SUCCESS);
}

struct parse_res read_str_many(const char *input, struct lisp_val *output) {
  struct reader r;
  reader_init(&r, input);

  struct list_builder builder;
  list_builder_init(&builder);

  enum parse_status res = P_SUCCESS;
  while (true) {
    res = reader_next(&r);
    if (res != P_SUCCESS) {
      break;
    }

    if (r.token.type == TOK_EOF) {
      break;
    }

    struct lisp_val next;
    res = read_form(&r, &next);
    if (res != P_SUCCESS) {
      break;
    }

    list_builder_append(&builder, next);
  }

  *output = list_build(&builder);
  if (res == P_SUCCESS) {
    return make_status_res(res);
  } else {
    return make_error_res(&r);
  }
}

/**
 * Exported function used in the core library.
 */
bool is_valid_symbol(const char *name) {
  // Leverage existing tokenizer
  struct reader r;
  reader_init(&r, name);

  enum parse_status res = read_next_atom(&r);
  if (res != P_SUCCESS) {
    return false;
  }
  if (r.token.type != TOK_SYMBOL) {
    return false;
  }

  return reader_at_eof(&r);
}

struct lisp_string *parse_error_format(const struct parse_error *error) {
  return lisp_string_format("parse error at %u:%u", error->error_pos.line + 1,
                            error->error_pos.col + 1);
}
