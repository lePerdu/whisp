#include "symbol.h"

#include <stddef.h>
#include <string.h>

#include "hash_table.h"
#include "memory.h"
#include "types.h"

static const struct lisp_vtable SYMBOL_VTABLE = {
    // TODO Mark as non-GC managed since they really aren't?
    .is_gc_managed = true,
    .name = "symbol",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

#define STR_HASH_FACTOR 31
// Only hash the beginning of strings to avoid extra cost for long strings
#define STR_HASH_MAX_LENGTH 32

/**
 * Basic string hashing function.
 */
static hash_t str_hash(const char *str, size_t length) {
  hash_t hash = 0;
  if (length > STR_HASH_MAX_LENGTH) {
    length = STR_HASH_MAX_LENGTH;
  }

  for (unsigned i = 0; i < length; i++) {
    hash = (hash * STR_HASH_FACTOR) + str[i];
  }
  return hash;
}

/**
 * Create un-interned symbol. Should only be used internally by the interning
 * mechanism.
 */
static struct lisp_symbol *lisp_symbol_create_uninterned(const char *name,
                                                         size_t length) {
  struct lisp_symbol *sym =
      lisp_obj_alloc(&SYMBOL_VTABLE, sizeof(*sym) + length + 1);
  sym->length = length;
  memcpy(sym->data, name, length);
  sym->data[length] = 0;
  sym->hash_code = str_hash(name, length);
  return sym;
}

struct lisp_symbol *lisp_symbol_create_cstr(const char *s) {
  return lisp_symbol_create(s, strlen(s));
}

bool lisp_val_is_symbol(struct lisp_val v) {
  return lisp_val_vtable(v) == &SYMBOL_VTABLE;
}

bool lisp_symbol_eq(const struct lisp_symbol *a, const struct lisp_symbol *b) {
#ifdef NDEBUG
  return a == b;
#else
  bool contents_equal =
      a->length == b->length && strncmp(a->data, b->data, a->length) == 0;
  // Symbols should never be semantically equal without being referentially
  // equal
  assert(contents_equal == (a == b));
  return contents_equal;
#endif
}

static void lisp_symbol_table_entry_visit(struct lisp_val v, visit_callback cb,
                                          void *ctx) {
  const struct lisp_hash_table_entry *e = lisp_val_as_obj(v);
  cb(ctx, e->key);
}

static const struct lisp_vtable SYMBOL_TABLE_ENTRY_VTABLE = {
    .is_gc_managed = true,
    .name = "hash-table-entry",
    .visit_children = lisp_symbol_table_entry_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_hash_table_entry *lisp_symbol_table_entry_create(
    struct lisp_symbol *sym) {
  gc_push_root_obj(sym);
  struct lisp_hash_table_entry *ent =
      lisp_obj_alloc(&SYMBOL_TABLE_ENTRY_VTABLE, sizeof(*ent));
  ent->key = lisp_val_from_obj(sym);
  ent->hash_code = sym->hash_code;
  gc_pop_root_expect_obj(sym);
  return ent;
}

static bool symbol_table_search(void *ctx, struct lisp_val key) {
  struct lisp_symbol *match_sym = ctx;
  struct lisp_symbol *key_sym = lisp_val_as_obj(key);
  return lisp_symbol_eq(match_sym, key_sym);
}

struct lisp_hash_table_entry *lisp_symbol_table_lookup(
    struct lisp_hash_table *map, struct lisp_symbol *key) {
  return lisp_hash_table_lookup(map, key->hash_code, symbol_table_search, key);
}

struct lisp_temp_symbol {
  const char *data;
  size_t length;
};

static bool symbol_table_search_name(void *ctx, struct lisp_val key) {
  struct lisp_temp_symbol *match_data = ctx;
  struct lisp_symbol *key_sym = lisp_val_as_obj(key);
  return key_sym->length == match_data->length &&
         memcmp(key_sym->data, match_data->data, match_data->length) == 0;
}

struct lisp_hash_table_entry *lisp_symbol_table_lookup_name(
    struct lisp_hash_table *map, const char *name, size_t length) {
  hash_t hash_code = str_hash(name, length);
  struct lisp_temp_symbol search_key = {
      .data = name,
      .length = length,
  };

  return lisp_hash_table_lookup(map, hash_code, symbol_table_search_name,
                                &search_key);
}

void lisp_symbol_table_insert(struct lisp_hash_table *map,
                              struct lisp_hash_table_entry *entry) {
  lisp_hash_table_insert(map, entry);
}

// TODO Change to 1 in GC_DEBUG mode to trigger reallocations more easily
#define SYMBOL_INTERN_INIT_CAP 32

static struct lisp_hash_table *symbol_intern_table = NULL;

struct lisp_val lisp_symbol_intern_table(void) {
  return lisp_val_from_obj(symbol_intern_table);
}

static void ensure_symbol_intern_table(void) {
  if (symbol_intern_table == NULL) {
    symbol_intern_table = lisp_hash_table_create(SYMBOL_INTERN_INIT_CAP);
  }
}

struct lisp_symbol *lisp_symbol_create(const char *name, size_t length) {
  ensure_symbol_intern_table();
  struct lisp_hash_table_entry *existing =
      lisp_symbol_table_lookup_name(symbol_intern_table, name, length);
  if (existing != NULL) {
    return lisp_val_as_obj(existing->key);
  }

  struct lisp_symbol *key = lisp_symbol_create_uninterned(name, length);
  struct lisp_hash_table_entry *entry = lisp_symbol_table_entry_create(key);
  lisp_symbol_table_insert(symbol_intern_table, entry);
  return key;
}

void lisp_symbol_intern_table_remove_if(bool (*pred)(struct lisp_val sym)) {
  if (symbol_intern_table == NULL) {
    return;
  }

  lisp_hash_table_remove_if(symbol_intern_table, pred);
}
