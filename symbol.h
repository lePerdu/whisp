#ifndef SYMBOL_H_
#define SYMBOL_H_

#include "types.h"

typedef unsigned hash_t;

/**
 * Symbol used for all identifiers.
 * These are always interned so that comparison is just a pointer comparison.
 */
struct lisp_symbol {
  struct lisp_obj header;
  // Hash code is computed at initialization since symbols are mainly used as
  // lookup keys
  hash_t hash_code;
  size_t length;
  char data[];
};

struct lisp_symbol *lisp_symbol_create(const char *s, size_t len);
bool lisp_val_is_symbol(struct lisp_val v);
/**
 * Create symbol from null-terminated string.
 *
 * The contents of the string are not checked for invalid characters/format,
 * so checking should be done up-front.
 */
struct lisp_symbol *lisp_symbol_create_cstr(const char *s);

static inline const char *lisp_symbol_name(const struct lisp_symbol *s) {
  return s->data;
}

bool lisp_symbol_eq(const struct lisp_symbol *a, const struct lisp_symbol *b);

struct lisp_hash_table_entry {
  struct lisp_obj header;
  struct lisp_hash_table_entry *next;
  struct lisp_symbol *key;
};

struct lisp_symbol_table {
  struct lisp_obj header;
  size_t capacity;
  size_t size;
  struct lisp_hash_table_entry *entries[];
};

struct lisp_symbol_table *lisp_symbol_table_create(size_t capacity);

struct lisp_hash_table_entry *lisp_symbol_table_lookup_symbol(
    struct lisp_symbol_table *map, const struct lisp_symbol *sym);
struct lisp_symbol_table *lisp_symbol_table_insert(
    struct lisp_symbol_table *map, struct lisp_hash_table_entry *entry);

// Utilities for interacting with the symbol intern table

struct lisp_symbol_table *lisp_symbol_intern_table(void);

// TODO This is very much a mix of concerns going on...
void lisp_symbol_intern_table_delete_if(bool (*pred)(struct lisp_symbol *sym));

#endif
