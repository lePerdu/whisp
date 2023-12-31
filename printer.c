#include "printer.h"

#include <ctype.h>
#include <float.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "bytecode.h"
#include "symbol.h"
#include "types.h"

static void print_cons(struct str_builder *b, struct lisp_cons *cons,
                       bool readble) {
  str_builder_append(b, '(');

  print_str_into(b, cons->car, readble);

  while (lisp_val_is_cons(cons->cdr)) {
    cons = lisp_val_as_obj(cons->cdr);
    str_builder_append(b, ' ');
    print_str_into(b, cons->car, readble);
  }

  if (!lisp_val_is_nil(cons->cdr)) {
    str_builder_concat_cstr(b, " . ");
    print_str_into(b, cons->cdr, readble);
  }

  str_builder_append(b, ')');
}

static void print_array(struct str_builder *b, struct lisp_array *arr,
                        bool readble) {
  str_builder_concat_cstr(b, "#<array");
  for (size_t i = 0; i < arr->length; i++) {
    str_builder_append(b, ' ');
    print_str_into(b, lisp_array_get(arr, i), readble);
  }
  str_builder_append(b, '>');
}

static void print_char_literal(struct str_builder *b, char c) {
  // TODO Unify code with reader
  str_builder_concat_cstr(b, "#\\");
  switch (c) {
    case 0:
      str_builder_concat_cstr(b, "null");
      break;
    case ' ':
      str_builder_concat_cstr(b, "space");
      break;
    case '\t':
      str_builder_concat_cstr(b, "tab");
      break;
    case '\n':
      str_builder_concat_cstr(b, "newline");
      break;
    default:
      str_builder_append(b, c);
      break;
  }
}

/**
 * Return translate character, or -1 if the character does not need to be
 * escaped.
 */
static int translate_escaped(char c) {
  switch (c) {
    case 0:
      return '0';
    case '"':
      return '"';
    case '\\':
      return '\\';
    case '\n':
      return 'n';
    case '\t':
      return 't';
    default:
      return -1;
  }
}

static void print_escaped(struct str_builder *b, struct lisp_string *s) {
  str_builder_append(b, '"');
  for (unsigned i = 0; i < s->length; i++) {
    char c = lisp_string_get(s, i);
    int escaped = translate_escaped(c);
    if (escaped == -1) {
      str_builder_append(b, c);
    } else {
      str_builder_append(b, '\\');
      str_builder_append(b, escaped);
    }
  }
  str_builder_append(b, '"');
}

static bool is_int_char(char c) { return isdigit(c) || c == '-' || c == '+'; }

/**
 * Customization over `printf` to ensure real numbers are always printed in a
 * way which differentiates them from integers.
 */
static void print_real(struct str_builder *b, lisp_real_t v) {
  int n_chars = str_builder_format(b, "%.*g", DBL_DIG, v);
  const struct lisp_string *str_buf = str_builder_get_str(b);

  // Make sure there is at least 1 char which indicates a real number
  size_t end_index = str_buf->length;
  size_t start_index = str_buf->length - n_chars;
  for (unsigned i = start_index; i < end_index; i++) {
    if (!is_int_char(lisp_string_get(str_buf, i))) {
      return;
    }
  }

  // Append ".0" to indicate it's a real number
  str_builder_concat_cstr(b, ".0");
}

static void print_function(struct str_builder *b, const char *name,
                           unsigned arg_count, bool variadic) {
  str_builder_concat_cstr(b, "#<function ");
  str_builder_concat_cstr(b, name);
  str_builder_append(b, ' ');
  if (arg_count == 0) {
    if (variadic) {
      str_builder_append(b, '_');
    } else {
      str_builder_concat_cstr(b, "()");
    }
  } else {
    str_builder_concat_cstr(b, "(_");
    for (unsigned i = 1; i < arg_count; i++) {
      str_builder_concat_cstr(b, " _");
    }

    if (variadic) {
      str_builder_concat_cstr(b, " . _");
    }

    str_builder_append(b, ')');
  }
  str_builder_append(b, '>');
}

void print_str_into(struct str_builder *b, struct lisp_val v, bool readable) {
  if (lisp_val_is_nil(v)) {
    str_builder_concat_cstr(b, "nil");
  } else if (lisp_val_is_int(v)) {
    str_builder_format(b, "%ld", lisp_val_as_int(v));
  } else if (lisp_val_is_real(v)) {
    print_real(b, lisp_val_as_real(v));
  } else if (lisp_val_is_char(v)) {
    char c = lisp_val_as_char(v);
    if (readable) {
      print_char_literal(b, c);
    } else {
      str_builder_append(b, c);
    }
  } else if (lisp_val_is_symbol(v)) {
    // TODO Utilize the known string length
    str_builder_concat_cstr(b, lisp_symbol_name(lisp_val_as_obj(v)));
  } else if (lisp_val_is_string(v)) {
    struct lisp_string *s = lisp_val_as_obj(v);
    if (readable) {
      print_escaped(b, s);
    } else {
      str_builder_concat(b, s);
    }
  } else if (lisp_val_is_cons(v)) {
    print_cons(b, lisp_val_as_obj(v), readable);
  } else if (lisp_val_is_func(v)) {
    {
      struct lisp_closure *closure = lisp_val_as_obj(v);
      const char *name = lisp_closure_name_cstr(closure);
      print_function(b, name, closure->code->req_arg_count,
                     closure->code->is_variadic);
    }
  } else if (lisp_val_is_atom(v)) {
    {
      const struct lisp_atom *atom = lisp_val_as_obj(v);
      str_builder_concat_cstr(b, "#<atom ");
      print_str_into(b, lisp_atom_deref(atom), readable);
      str_builder_append(b, '>');
    }
  } else if (lisp_val_is_array(v)) {
    print_array(b, lisp_val_as_obj(v), readable);
  } else {
    str_builder_concat_cstr(b, "#<");
    str_builder_concat_cstr(b, lisp_val_vtable(v)->name);
    str_builder_append(b, '>');
  }
}

struct lisp_string *print_str(struct lisp_val v, bool readble) {
  struct str_builder builder;
  str_builder_init(&builder);
  print_str_into(&builder, v, readble);
  return str_build(&builder);
}

void display_str(const struct lisp_string *s) {
  for (unsigned i = 0; i < s->length; i++) {
    putchar(lisp_string_get(s, i));
  }
}
