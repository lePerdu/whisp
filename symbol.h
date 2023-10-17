#ifndef SYMBOL_H_
#define SYMBOL_H_

#include <stddef.h>

#include "hash_table.h"
#include "types.h"

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

struct lisp_hash_table_entry *lisp_symbol_table_entry_create(
    struct lisp_symbol *sym);

struct lisp_hash_table_entry *lisp_symbol_table_lookup(
    struct lisp_hash_table *map, struct lisp_symbol *key);
struct lisp_hash_table_entry *lisp_symbol_table_lookup_name(
    struct lisp_hash_table *map, const char *name, size_t length);

void lisp_symbol_table_insert(struct lisp_hash_table *map,
                              struct lisp_hash_table_entry *entry);

// Utilities for interacting with the symbol intern table
// TODO This is very much a mix of concerns going on...

struct lisp_val lisp_symbol_intern_table(void);
void lisp_symbol_intern_table_remove_if(bool (*pred)(struct lisp_val sym));

#endif
