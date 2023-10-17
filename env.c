#include "env.h"

#include "hash_table.h"
#include "memory.h"
#include "symbol.h"
#include "types.h"

// TODO Change to 1 in GC_DEBUG mode to trigger reallocations more easily
#define ENV_MAP_INIT_CAP 32

struct lisp_env {
  struct lisp_obj header;
  struct lisp_hash_table *mappings;
};

static void lisp_env_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  struct lisp_env *env = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(env->mappings));
}

static const struct lisp_vtable ENV_VTABLE = {
    .is_gc_managed = true,
    .name = "environment",
    .visit_children = lisp_env_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_env_binding {
  struct lisp_hash_table_entry ht;
  struct lisp_val value;
  bool is_macro;
};

static void lisp_env_binding_visit(struct lisp_val v, visit_callback cb,
                                   void *ctx) {
  const struct lisp_env_binding *e = lisp_val_as_obj(v);
  cb(ctx, e->ht.key);
  cb(ctx, e->value);
}

bool lisp_val_is_env(struct lisp_val v) {
  return lisp_val_vtable(v) == &ENV_VTABLE;
}

static const struct lisp_vtable ENV_TABLE_ENTRY_VTABLE = {
    .is_gc_managed = true,
    .name = "environment-entry",
    .visit_children = lisp_env_binding_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_env *lisp_env_create(void) {
  struct lisp_hash_table *mappings = lisp_hash_table_create(ENV_MAP_INIT_CAP);
  gc_push_root_obj(mappings);

  struct lisp_env *env = lisp_obj_alloc(&ENV_VTABLE, sizeof(*env));
  env->mappings = mappings;
  gc_pop_root_expect_obj(mappings);

  return env;
}

struct lisp_env_binding *lisp_env_get(struct lisp_env *env,
                                      struct lisp_symbol *sym) {
  return (struct lisp_env_binding *)lisp_symbol_table_lookup(env->mappings,
                                                             sym);
}

static struct lisp_env_binding *lisp_env_set_or_insert(struct lisp_env *env,
                                                       struct lisp_symbol *sym,
                                                       struct lisp_val value,
                                                       bool is_macro) {
  struct lisp_env_binding *entry =
      (struct lisp_env_binding *)lisp_symbol_table_lookup(env->mappings, sym);
  if (entry != NULL) {
    entry->value = value;
    entry->is_macro = is_macro;
  }

  gc_push_root_obj(sym);
  gc_push_root(value);

  entry = lisp_obj_alloc(&ENV_TABLE_ENTRY_VTABLE, sizeof(*entry));
  entry->ht.key = lisp_val_from_obj(sym);
  entry->ht.hash_code = sym->hash_code;
  entry->value = value;
  entry->is_macro = is_macro;

  gc_pop_root_expect(value);
  gc_pop_root_expect_obj(sym);

  lisp_symbol_table_insert(env->mappings,
                           (struct lisp_hash_table_entry *)entry);
  return entry;
}

struct lisp_env_binding *lisp_env_set(struct lisp_env *env,
                                      struct lisp_symbol *sym,
                                      struct lisp_val val) {
  return lisp_env_set_or_insert(env, sym, val, false);
}

struct lisp_env_binding *lisp_env_set_macro(struct lisp_env *env,
                                            struct lisp_symbol *sym,
                                            struct lisp_val val) {
  return lisp_env_set_or_insert(env, sym, val, true);
}

bool lisp_val_is_env_binding(struct lisp_val v) {
  return lisp_val_vtable(v) == &ENV_TABLE_ENTRY_VTABLE;
}

const struct lisp_symbol *lisp_env_binding_name(
    const struct lisp_env_binding *b) {
  return lisp_val_as_obj(b->ht.key);
}

struct lisp_val lisp_env_binding_value(const struct lisp_env_binding *b) {
  return b->value;
}

void lisp_env_binding_set_value(struct lisp_env_binding *b,
                                struct lisp_val new) {
  b->value = new;
}

bool lisp_env_binding_is_macro(const struct lisp_env_binding *b) {
  return b->is_macro;
}
