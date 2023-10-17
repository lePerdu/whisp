#include "symbol.h"

#include <string.h>

#include "memory.h"

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

static void lisp_hash_table_entry_visit(struct lisp_val v, visit_callback cb,
                                        void *ctx) {
  const struct lisp_hash_table_entry *e = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(e->key));
}

static const struct lisp_vtable HASH_TABLE_ENTRY_VTABLE = {
    .is_gc_managed = true,
    .name = "hash-table-entry",
    .visit_children = lisp_hash_table_entry_visit,
    .destroy = lisp_destroy_none,
};

static void lisp_symbol_table_visit(struct lisp_val v, visit_callback cb,
                                    void *ctx) {
  const struct lisp_symbol_table *table = lisp_val_as_obj(v);
  // This is necessary because the GC can be run (at least in debug mode)
  // before the symbol table exists (because the symbol table is allocated by
  // the GC)
  // TODO Is there a cleaner way to do this?
  if (table == NULL) {
    return;
  }

  for (unsigned i = 0; i < table->capacity; i++) {
    struct lisp_hash_table_entry *entry = table->entries[i];
    while (entry != NULL) {
      cb(ctx, lisp_val_from_obj(entry));
      entry = entry->next;
    }
  }
}

static const struct lisp_vtable SYMBOL_TABLE_VTABLE = {
    .is_gc_managed = true,
    .name = "symbol_table",
    .visit_children = lisp_symbol_table_visit,
    // TODO Make the symbol table separately allocate its hash table entries?
    .destroy = lisp_destroy_none,
};

struct lisp_symbol_table *lisp_symbol_table_create(size_t capacity) {
  // Size must be a power of 2
  assert(((capacity - 1) & capacity) == 0);
  struct lisp_symbol_table *map = lisp_obj_alloc(
      &SYMBOL_TABLE_VTABLE,
      sizeof(*map) + sizeof(struct lisp_hash_table_entry *) * capacity);
  map->capacity = capacity;
  map->size = 0;
  memset(map->entries, 0, sizeof(struct lisp_hash_table_entry *) * capacity);
  return map;
}

static struct lisp_hash_table_entry *symbol_table_lookup_entry(
    struct lisp_symbol_table *map, const char *key, size_t key_length) {
  hash_t hash_code = str_hash(key, key_length);
  unsigned bucket = hash_code & (map->capacity - 1);
  struct lisp_hash_table_entry *entry = map->entries[bucket];
  while (entry != NULL) {
    if (entry->key->hash_code == hash_code &&
        strncmp(lisp_symbol_name(entry->key), key, key_length) == 0) {
      return entry;
    }
    entry = entry->next;
  }
  return NULL;
}

/**
 * More optimized lookup which relies on symbol interning.
 */
struct lisp_hash_table_entry *lisp_symbol_table_lookup_symbol(
    struct lisp_symbol_table *map, const struct lisp_symbol *sym) {
  unsigned bucket = sym->hash_code & (map->capacity - 1);
  struct lisp_hash_table_entry *entry = map->entries[bucket];
  while (entry != NULL) {
    // lisp_symbol_eq just checks identity since symbols are interned
    if (lisp_symbol_eq(entry->key, sym)) {
      return entry;
    }
    entry = entry->next;
  }
  return NULL;
}

static inline bool should_rehash(size_t size, size_t capacity) {
  return size >= capacity * 3 / 4;
}

/**
 * Insert an entry into the table.
 *
 * @return the modified table. (The original table reference should be
 * considered invalid)
 */
static void lisp_symbol_table_do_insert(struct lisp_symbol_table *map,
                                        struct lisp_hash_table_entry *new_ent) {
  unsigned bucket = new_ent->key->hash_code & (map->capacity - 1);
  new_ent->next = map->entries[bucket];
  map->entries[bucket] = new_ent;
  map->size++;
}

static struct lisp_symbol_table *lisp_symbol_table_resize(
    struct lisp_symbol_table *map) {
  struct lisp_symbol_table *new = lisp_symbol_table_create(map->capacity * 2);
  for (unsigned i = 0; i < map->capacity; i++) {
    while (map->entries[i] != NULL) {
      struct lisp_hash_table_entry *entry = map->entries[i];
      map->entries[i] = entry->next;

      lisp_symbol_table_do_insert(new, entry);
    }
  }
  return new;
}

/**
 * Insert a new entry into the table. The entry should be GC-allocated. Only the
 * key need be initialized.
 */
struct lisp_symbol_table *lisp_symbol_table_insert(
    struct lisp_symbol_table *map, struct lisp_hash_table_entry *entry) {
  // TODO Should the map be pushed? Is it expected to always be marked for other
  // reasons?
  gc_push_root_obj(map);
  gc_push_root_obj(entry);

  if (should_rehash(map->size, map->capacity)) {
    map = lisp_symbol_table_resize(map);
  }

  gc_pop_root_expect_obj(entry);
  gc_pop_root();

  lisp_symbol_table_do_insert(map, entry);
  return map;
}

/*
static void lisp_symbol_table_delete(struct lisp_symbol_table *map,
                                     const struct lisp_symbol *key) {
  hash_t hash_code = lisp_symbol_hash(key);
  unsigned bucket = hash_code & (map->capacity - 1);
  struct lisp_hash_table_entry **entry = &map->entries[bucket];
  while (*entry != NULL) {
    if ((*entry)->hash_code == hash_code &&
        lisp_symbol_eq((*entry)->key, key)) {
      *entry = (*entry)->next;
      map->size--;
      break;
    }
    entry = &(*entry)->next;
  }
}
*/

// TODO Change to 1 in GC_DEBUG mode to trigger reallocations more easily
#define SYMBOL_INTEN_INIT_CAP 32

static struct lisp_symbol_table *symbol_intern_table = NULL;

struct lisp_symbol_table *lisp_symbol_intern_table(void) {
  return symbol_intern_table;
}

static void ensure_symbol_intern_table(void) {
  if (symbol_intern_table == NULL) {
    symbol_intern_table = lisp_symbol_table_create(SYMBOL_INTEN_INIT_CAP);
  }
}

struct lisp_symbol *lisp_symbol_create(const char *name, size_t length) {
  ensure_symbol_intern_table();
  struct lisp_hash_table_entry *existing =
      symbol_table_lookup_entry(symbol_intern_table, name, length);
  if (existing != NULL) {
    return existing->key;
  }

  struct lisp_symbol *key = lisp_symbol_create_uninterned(name, length);

  gc_push_root_obj(key);
  struct lisp_hash_table_entry *entry =
      lisp_obj_alloc(&HASH_TABLE_ENTRY_VTABLE, sizeof(*entry));
  entry->key = key;
  gc_pop_root_expect_obj(key);

  symbol_intern_table = lisp_symbol_table_insert(symbol_intern_table, entry);
  return entry->key;
}

void lisp_symbol_intern_table_delete_if(bool (*pred)(struct lisp_symbol *sym)) {
  if (symbol_intern_table == NULL) {
    return;
  }

  for (unsigned i = 0; i < symbol_intern_table->capacity; i++) {
    struct lisp_hash_table_entry **entry = &symbol_intern_table->entries[i];
    while (*entry != NULL) {
      if (pred((*entry)->key)) {
        // Replace the pointer in the linked list
        *entry = (*entry)->next;
        symbol_intern_table->size--;
      } else {
        // Just advance the local variable
        entry = &(*entry)->next;
      }
    }
  }
}
