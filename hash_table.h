#ifndef HASH_TABLE_H_
#define HASH_TABLE_H_

#include <stddef.h>

#include "types.h"

typedef unsigned hash_t;

struct lisp_hash_table_entry {
  struct lisp_obj header;
  struct lisp_hash_table_entry *next;
  hash_t hash_code;
  struct lisp_val key;
};

/**
 * Low level hash table representation used in construction of higher-level
 * abstractions.
 */
struct lisp_hash_table {
  struct lisp_obj header;
  size_t capacity;
  size_t size;
  struct lisp_hash_table_entry **entries;
};

struct lisp_hash_table *lisp_hash_table_create(size_t capacity);

struct lisp_hash_table_entry *lisp_hash_table_lookup(
    struct lisp_hash_table *map, hash_t hash_code,
    bool (*search)(void *ctx, struct lisp_val key), void *ctx);

/**
 * Insert a new entry into the table. The entry should be GC-allocated.
 *
 * Note: this does not check for existing mappings.
 */
void lisp_hash_table_insert(struct lisp_hash_table *map,
                            struct lisp_hash_table_entry *entry);

void lisp_hash_table_remove_if(struct lisp_hash_table *map,
                               bool (*pred)(struct lisp_val key));

hash_t hash_string(const char *str, size_t length);
hash_t hash_lisp_val(struct lisp_val v);

#endif
