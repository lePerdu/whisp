#include "hash_table.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "types.h"

static void lisp_hash_table_visit(struct lisp_val v, visit_callback cb,
                                  void *ctx) {
  const struct lisp_hash_table *table = lisp_val_as_obj(v);
  for (unsigned i = 0; i < table->capacity; i++) {
    struct lisp_hash_table_entry *entry = table->entries[i];
    while (entry != NULL) {
      cb(ctx, lisp_val_from_obj(entry));
      entry = entry->next;
    }
  }
}

static void lisp_hash_table_destroy(struct lisp_val v) {
  const struct lisp_hash_table *table = lisp_val_as_obj(v);
  free(table->entries);
}

static const struct lisp_vtable HASH_TABLE_VTABLE = {
    .is_gc_managed = true,
    .name = "hash_table",
    .visit_children = lisp_hash_table_visit,
    .destroy = lisp_hash_table_destroy,
};

struct lisp_hash_table *lisp_hash_table_create(size_t capacity) {
  // Size must be a power of 2
  assert(((capacity - 1) & capacity) == 0);
  struct lisp_hash_table *map = lisp_obj_alloc(
      &HASH_TABLE_VTABLE,
      sizeof(*map) + sizeof(struct lisp_hash_table_entry *) * capacity);
  map->capacity = capacity;
  map->size = 0;
  map->entries = calloc(capacity, sizeof(struct lisp_hash_table_entry *));
  return map;
}

struct lisp_hash_table_entry *lisp_hash_table_lookup(
    struct lisp_hash_table *map, hash_t hash_code,
    bool (*search)(void *ctx, struct lisp_val key), void *ctx) {
  unsigned bucket = hash_code & (map->capacity - 1);
  struct lisp_hash_table_entry *entry = map->entries[bucket];
  while (entry != NULL) {
    if (entry->hash_code == hash_code && search(ctx, entry->key)) {
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
static void lisp_hash_table_do_insert(struct lisp_hash_table *map,
                                      struct lisp_hash_table_entry *new_ent) {
  unsigned bucket = new_ent->hash_code & (map->capacity - 1);
  new_ent->next = map->entries[bucket];
  map->entries[bucket] = new_ent;
  map->size++;
}

static void lisp_hash_table_resize(struct lisp_hash_table *map) {
  // The reference is temporarily "detached", so this function has to be careful
  // to not allocate any GC objects
  size_t old_capacity = map->capacity;
  struct lisp_hash_table_entry **old_entries = map->entries;

  map->size = 0;
  map->capacity *= 2;
  map->entries = calloc(map->capacity, sizeof(struct lisp_hash_table_entry *));

  for (unsigned i = 0; i < old_capacity; i++) {
    while (old_entries[i] != NULL) {
      struct lisp_hash_table_entry *entry = old_entries[i];
      old_entries[i] = entry->next;

      lisp_hash_table_do_insert(map, entry);
    }
  }
}

void lisp_hash_table_insert(struct lisp_hash_table *map,
                            struct lisp_hash_table_entry *entry) {
  if (should_rehash(map->size, map->capacity)) {
    lisp_hash_table_resize(map);
  }
  lisp_hash_table_do_insert(map, entry);
}

void lisp_hash_table_remove_if(struct lisp_hash_table *map,
                               bool (*pred)(struct lisp_val key)) {
  for (unsigned i = 0; i < map->capacity; i++) {
    struct lisp_hash_table_entry **entry = &map->entries[i];
    while (*entry != NULL) {
      if (pred((*entry)->key)) {
        // Replace the pointer in the linked list
        *entry = (*entry)->next;
        map->size--;
      } else {
        // Just advance the local variable
        entry = &(*entry)->next;
      }
    }
  }
}
