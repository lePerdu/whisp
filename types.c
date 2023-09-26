#include "types.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"

enum ptr_tag {
  TAG_INT = 1,
  TAG_OBJ = 0,
};

#define TAG_SHIFT 1
#define TAG_MASK ((uintptr_t)1)
#define VALUE_MASK (~TAG_MASK)
#define REQUIRED_ALIGN (TAG_MASK + 1)

static_assert(alignof(struct lisp_obj) >= REQUIRED_ALIGN,
              "Alignment not big enough for tagged pointers");

static inline bool lisp_val_is_obj(struct lisp_val v) {
  return (v.tagged_ptr & TAG_MASK) == TAG_OBJ;
}

inline void *lisp_val_as_obj(struct lisp_val v) {
  assert(lisp_val_is_obj(v));
  // Since the object tag is 0, this can be a no-op
  static_assert(TAG_OBJ == 0, "Object tag is non-0");
  return (void *)v.tagged_ptr;
}

inline struct lisp_val lisp_val_from_obj(void *obj) {
  uintptr_t raw = (uintptr_t)obj;
  assert((raw & TAG_MASK) == 0);
  // Since the object tag is 0, this can be a no-op
  static_assert(TAG_OBJ == 0, "Object tag is non-0");
  return (struct lisp_val){raw};
}

void *lisp_obj_alloc(const struct lisp_vtable *vt, size_t size) {
  struct lisp_obj *obj = lisp_alloc(size);
  obj->vt = vt;
  return obj;
}

static bool lisp_type_valid(enum lisp_type t) {
  return LISP_TYPE_MIN <= t && t < LISP_TYPE_MAX;
}

static void visit_none(struct lisp_val object, visit_callback cb, void *ctx) {
  (void)object;
  (void)cb;
  (void)ctx;
}

static void destroy_none(struct lisp_val object) { (void)object; }

static const struct lisp_vtable INT_VTABLE = {
    .type = LISP_INT,
    .is_gc_managed = false,
    .name = "int",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

static const struct lisp_vtable NIL_VTABLE = {
    .type = LISP_NIL,
    .is_gc_managed = false,
    .name = "nil",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

// There's redundant structure here, but otherwise there are redundant checks.
// The solution is probably relying more on vtables rather than types or
// vice-versa.

const struct lisp_vtable *lisp_val_vtable(struct lisp_val v) {
  if ((v.tagged_ptr & TAG_MASK) == TAG_INT) {
    return &INT_VTABLE;
  } else if (v.tagged_ptr == LISP_VAL_NIL.tagged_ptr) {
    return &NIL_VTABLE;
  } else {
    return LISP_VAL_AS(struct lisp_obj, v)->vt;
  }
}

enum lisp_type lisp_val_type(struct lisp_val v) {
  if ((v.tagged_ptr & TAG_MASK) == TAG_INT) {
    return LISP_INT;
  } else if (v.tagged_ptr == LISP_VAL_NIL.tagged_ptr) {
    return LISP_NIL;
  } else {
    enum lisp_type t = LISP_VAL_AS(struct lisp_obj, v)->vt->type;
    assert(lisp_type_valid(t));
    return t;
  }
}

const char *lisp_val_type_name(struct lisp_val v) {
  return lisp_val_vtable(v)->name;
}

void *lisp_val_cast(enum lisp_type type, struct lisp_val v) {
  assert(type != LISP_NIL);
  assert(type != LISP_INT);

  if (lisp_val_type(v) != type) {
    return NULL;
  }
  return lisp_val_as_obj(v);
}

// TODO Make a pointer to an actual object? Would have to fix
// `lisp_val_from_obj` to account for C NULL and/or make sure NULL is never
// passed to it. = lisp_val_from_obj(NULL)
const struct lisp_val LISP_VAL_NIL = {TAG_OBJ};

bool lisp_val_is_nil(struct lisp_val v) { return lisp_val_type(v) == LISP_NIL; }

long lisp_val_as_int(struct lisp_val v) {
  assert((v.tagged_ptr & TAG_MASK) == TAG_INT);
  return ((long)v.tagged_ptr) >> TAG_SHIFT;
}

struct lisp_val lisp_val_from_int(long n) {
  // Having the assertions aborts the program on integer overflow.
  // TODO Make a runtime error? Just ignore overflow?
  // assert(n >= LISP_INT_MIN);
  // assert(n <= LISP_INT_MAX);
  return (struct lisp_val){(uintptr_t)(n << TAG_SHIFT) | TAG_INT};
}

struct lisp_val lisp_true(void) {
  // This only works because true is referenced by the global environment. If it
  // isn't this would segfault.
  // TODO Fix this.
  static struct lisp_symbol *cached_sym = NULL;
  if (cached_sym == NULL) {
    cached_sym = lisp_symbol_create_cstr("true");
  }
  return lisp_val_from_obj(cached_sym);
}

struct lisp_val lisp_false(void) { return LISP_VAL_NIL; }

bool lisp_val_is_true(struct lisp_val v) { return !lisp_val_is_false(v); }

bool lisp_val_is_false(struct lisp_val v) {
  return lisp_val_type(v) == LISP_NIL;
}

bool lisp_val_is_number(struct lisp_val v) {
  enum lisp_type type = lisp_val_type(v);
  return type == LISP_INT || type == LISP_REAL;
}

bool lisp_val_is_list(struct lisp_val v) {
  while (lisp_val_type(v) == LISP_CONS) {
    v = LISP_VAL_AS(struct lisp_cons, v)->cdr;
  }
  return lisp_val_type(v) == LISP_NIL;
}

bool lisp_val_is_func(struct lisp_val v) {
  enum lisp_type type = lisp_val_type(v);
  return type == LISP_CLOSURE;
}

struct lisp_real {
  struct lisp_obj header;
  double value;
};

static const struct lisp_vtable REAL_VTABLE = {
    .type = LISP_REAL,
    .is_gc_managed = true,
    .name = "real",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

struct lisp_val lisp_val_from_real(double d) {
  struct lisp_real *v = lisp_obj_alloc(&REAL_VTABLE, sizeof(*v));
  v->value = d;
  return lisp_val_from_obj(v);
}

double lisp_val_as_real(struct lisp_val v) {
  assert(lisp_val_type(v) == LISP_REAL);
  return LISP_VAL_AS(struct lisp_real, v)->value;
}

struct lisp_char {
  struct lisp_obj header;
  lisp_char_t value;
};

static const struct lisp_vtable CHAR_VTABLE = {
    .type = LISP_CHAR,
    // TODO Figure out a way to avoid heap allocating these?
    // For strictly 8-bit characters, they could be statically allocated, but
    // not when e.g. UTF-8 chars are supported
    .is_gc_managed = true,
    .name = "char",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

struct lisp_val lisp_val_from_char(lisp_char_t c) {
  struct lisp_char *v = lisp_obj_alloc(&CHAR_VTABLE, sizeof(*v));
  v->value = c;
  return lisp_val_from_obj(v);
}

lisp_char_t lisp_val_as_char(struct lisp_val v) {
  assert(lisp_val_type(v) == LISP_CHAR);
  return LISP_VAL_AS(struct lisp_char, v)->value;
}

bool lisp_val_identical(struct lisp_val a, struct lisp_val b) {
  return a.tagged_ptr == b.tagged_ptr;
}

struct lisp_atom {
  struct lisp_obj header;
  struct lisp_val value;
};

static void lisp_atom_visit(struct lisp_val a, visit_callback cb, void *ctx) {
  cb(ctx, LISP_VAL_AS(struct lisp_atom, a)->value);
}

static const struct lisp_vtable ATOM_VTABLE = {
    .type = LISP_ATOM,
    .is_gc_managed = true,
    .name = "atom",
    .visit_children = lisp_atom_visit,
    .destroy = destroy_none,
};

struct lisp_atom *lisp_atom_create(struct lisp_val v) {
  gc_push_root(v);
  struct lisp_atom *atom = lisp_obj_alloc(&ATOM_VTABLE, sizeof(*atom));
  lisp_atom_reset(atom, v);
  gc_pop_root_expect(v);
  return atom;
}

struct lisp_val lisp_atom_deref(const struct lisp_atom *a) { return a->value; }

void lisp_atom_reset(struct lisp_atom *a, struct lisp_val v) { a->value = v; }

struct lisp_string {
  struct lisp_obj header;
  size_t size;
  char data[];
};

static const struct lisp_vtable STRING_VTABLE = {
    .type = LISP_STRING,
    .is_gc_managed = true,
    .name = "string",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

static struct lisp_string *lisp_string_alloc(size_t capacity) {
  struct lisp_string *s = lisp_obj_alloc(&STRING_VTABLE, sizeof(*s) + capacity);
  s->size = 0;
  return s;
}

struct lisp_string *lisp_string_create(const char *s, size_t len) {
  struct lisp_string *str =
      lisp_obj_alloc(&STRING_VTABLE, sizeof(*str) + len + 1);
  str->size = len;
  memcpy(str->data, s, len);
  str->data[len] = 0;

  return str;
}

struct lisp_string *lisp_string_create_cstr(const char *s) {
  return lisp_string_create(s, strlen(s));
}

struct lisp_string *lisp_string_vformat(const char *format, va_list ap) {
  // Call vsnprintf with NULL buffer first to get the required buffer size,
  // then allocate the string and run it again.
  va_list ap_check_size;
  va_copy(ap_check_size, ap);
  int n_chars = vsnprintf(NULL, 0, format, ap_check_size);
  va_end(ap_check_size);

  // TODO Return NULL instead?
  assert(n_chars >= 0);

  size_t cap = n_chars + 1;
  struct lisp_string *str = lisp_string_alloc(cap);

  int n_chars_actual = vsnprintf(str->data, cap, format, ap);
  str->size = n_chars;

  assert(n_chars_actual == n_chars);
  (void)n_chars_actual;
  return str;
}

struct lisp_string *lisp_string_format(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  struct lisp_string *str = lisp_string_vformat(format, ap);
  va_end(ap);
  return str;
}

size_t lisp_string_length(const struct lisp_string *s) { return s->size; }

char lisp_string_get(const struct lisp_string *s, size_t i) {
  // <= to allow fetching the null byte
  assert(i <= s->size);
  return s->data[i];
}

const char *lisp_string_as_cstr(const struct lisp_string *s) { return s->data; }

bool lisp_string_eq(const struct lisp_string *a, const struct lisp_string *b) {
  if (a->size != b->size) {
    return false;
  }
  return memcmp(a->data, b->data, a->size) == 0;
}

#define STR_BUILDER_INIT_CAP 8

void str_builder_init(struct str_builder *b) {
  str_builder_init_cap(b, STR_BUILDER_INIT_CAP);
}

void str_builder_init_cap(struct str_builder *b, size_t capacity) {
  // Include null byte
  b->capacity = capacity + 1;
  b->str_atom =
      lisp_atom_create(lisp_val_from_obj(lisp_string_alloc(b->capacity)));
  gc_push_root_obj(b->str_atom);
}

static struct lisp_string *str_builder_get(struct str_builder *b) {
  return lisp_val_as_obj(lisp_atom_deref(b->str_atom));
}

const struct lisp_string *str_builder_get_str(const struct str_builder *b) {
  return str_builder_get((struct str_builder *)b);
}

static struct lisp_string *str_builder_ensure_cap(struct str_builder *b,
                                                  size_t added_size) {
  struct lisp_string *str = str_builder_get(b);
  size_t new_size = str->size + added_size;
  if (new_size > b->capacity) {
    // TODO Avoid loop and just do max(cap*2, new_size)?
    do {
      b->capacity *= 2;
    } while (new_size > b->capacity);
    struct lisp_string *new_str = lisp_string_alloc(b->capacity);
    new_str->size = str->size;
    memcpy(new_str->data, str->data, str->size);
    lisp_atom_reset(b->str_atom, lisp_val_from_obj(new_str));
    return new_str;
  } else {
    return str;
  }
}

void str_builder_append(struct str_builder *b, char c) {
  struct lisp_string *str = str_builder_ensure_cap(b, 1);
  str->data[str->size++] = c;
}

void str_builder_concat(struct str_builder *b, const struct lisp_string *s) {
  // TODO Save the string in case the buffer is reallocated?
  // gc_push_root_obj((void *)s);
  str_builder_concat_n(b, s->data, s->size);
  // gc_pop_root_expect_obj((void *)s);
}

void str_builder_concat_cstr(struct str_builder *b, const char *cstr) {
  while (*cstr != 0) {
    str_builder_append(b, *cstr);
    cstr++;
  }
}

void str_builder_concat_n(struct str_builder *b, const char *buf, size_t n) {
  struct lisp_string *str = str_builder_ensure_cap(b, n);
  memcpy(&str->data[str->size], buf, n);
  str->size += n;
}

int str_builder_format(struct str_builder *b, const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  int result = str_builder_vformat(b, format, ap);
  va_end(ap);
  return result;
}

int str_builder_vformat(struct str_builder *b, const char *format, va_list ap) {
  struct lisp_string *str = str_builder_get(b);
  // Try to build with existing capacity first
  char *format_buf = &str->data[str->size];
  size_t remaining_cap = b->capacity - str->size;

  // Call vsnprintf with NULL buffer first to get the required buffer
  // size, then allocate the string and run it again.
  va_list ap_check_size;
  va_copy(ap_check_size, ap);
  int n_chars = vsnprintf(format_buf, remaining_cap, format, ap_check_size);
  va_end(ap_check_size);

  if (n_chars < 0) {
    return n_chars;
  }

  // Resize and try again
  // In the equal case snprintf wanted to write an extra NULL byte. Even though
  // str_builder handles NULL terminators itself, snprintf may not write the
  // full content if it can't write the NULL byte.
  if ((unsigned)n_chars >= remaining_cap) {
    size_t req_cap = n_chars + 1;
    str = str_builder_ensure_cap(b, req_cap);
    format_buf = &str->data[str->size];

    int n_chars_actual = vsnprintf(format_buf, req_cap, format, ap);
    assert(n_chars_actual == n_chars);
    (void)n_chars_actual;
  }

  str->size += n_chars;
  return n_chars;
}

struct lisp_string *str_build(struct str_builder *b) {
  // Add in null byte, but don't count in string size
  struct lisp_string *str = str_builder_ensure_cap(b, 1);
  str->data[str->size] = 0;

  gc_pop_root_expect_obj(b->str_atom);
  return str;
}

typedef unsigned hash_t;

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

struct lisp_symbol {
  struct lisp_obj header;
  // Hash code is computed at initialization since symbols are mainly used as
  // lookup keys
  hash_t hash_code;
  size_t size;
  char data[];
};

static const struct lisp_vtable SYMBOL_VTABLE = {
    .type = LISP_SYMBOL,
    // TODO Mark as non-GC managed since they really aren't?
    .is_gc_managed = true,
    .name = "symbol",
    .visit_children = visit_none,
    .destroy = destroy_none,
};

/**
 * Create un-interned symbol. Should only be used internally by the interning
 * mechanism.
 */
static struct lisp_symbol *lisp_symbol_create_uninterned(const char *name,
                                                         size_t length) {
  struct lisp_symbol *sym =
      lisp_obj_alloc(&SYMBOL_VTABLE, sizeof(*sym) + length + 1);
  sym->size = length;
  memcpy(sym->data, name, length);
  sym->data[length] = 0;
  sym->hash_code = str_hash(name, length);
  return sym;
}

struct lisp_symbol *lisp_symbol_create_cstr(const char *s) {
  return lisp_symbol_create(s, strlen(s));
}

const char *lisp_symbol_name(const struct lisp_symbol *s) { return s->data; }

size_t lisp_symbol_length(const struct lisp_symbol *s) { return s->size; }

bool lisp_symbol_eq(const struct lisp_symbol *a, const struct lisp_symbol *b) {
#ifdef NDEBUG
  return a == b;
#else
  bool contents_equal =
      a->size == b->size && strncmp(a->data, b->data, a->size) == 0;
  // Symbols should never be semantically equal without being referentially
  // equal
  assert(contents_equal == (a == b));
  return contents_equal;
#endif
}

static void lisp_cons_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  const struct lisp_cons *cons = lisp_val_as_obj(v);
  cb(ctx, cons->car);
  cb(ctx, cons->cdr);
}

static const struct lisp_vtable CONS_VTABLE = {
    .type = LISP_CONS,
    .is_gc_managed = true,
    .name = "cons",
    .visit_children = lisp_cons_visit,
    .destroy = destroy_none,
};

struct lisp_cons *lisp_cons_create(struct lisp_val car, struct lisp_val cdr) {
  gc_push_root(car);
  gc_push_root(cdr);
  struct lisp_cons *cons = lisp_obj_alloc(&CONS_VTABLE, sizeof(*cons));
  cons->car = car;
  cons->cdr = cdr;
  gc_pop_root_expect(cdr);
  gc_pop_root_expect(car);
  return cons;
}

unsigned lisp_list_count(struct lisp_val list) {
  unsigned count = 0;
  while (lisp_val_type(list) == LISP_CONS) {
    list = LISP_VAL_AS(struct lisp_cons, list)->cdr;
    count++;
  }
  assert(lisp_val_type(list) == LISP_NIL);
  return count;
}

void list_builder_init(struct list_builder *b) {
  b->list_atom = lisp_atom_create(LISP_VAL_NIL);
  b->tail = NULL;
  gc_push_root_obj(b->list_atom);
}

void list_builder_append(struct list_builder *b, struct lisp_val v) {
  struct lisp_cons *node = lisp_cons_create(v, LISP_VAL_NIL);

  struct lisp_val node_val = lisp_val_from_obj(node);
  if (lisp_val_is_nil(lisp_atom_deref(b->list_atom))) {
    b->tail = node;
    lisp_atom_reset(b->list_atom, node_val);
  } else {
    b->tail->cdr = node_val;
    b->tail = node;
  }
}

void list_builder_end_pair(struct list_builder *b, struct lisp_val pair_end) {
  assert(b->tail != NULL);
  b->tail->cdr = pair_end;
}

struct lisp_val list_builder_current(const struct list_builder *b) {
  return lisp_atom_deref(b->list_atom);
}

struct lisp_val list_build(struct list_builder *b) {
  gc_pop_root_expect_obj(b->list_atom);
  return lisp_atom_deref(b->list_atom);
}

void list_mapper_init(struct list_mapper *m, struct lisp_val original) {
  m->original = original;
  enum lisp_type original_type = lisp_val_type(original);
  m->cursor = original_type == LISP_CONS ? lisp_val_as_obj(original) : NULL;
  m->copied = false;
  m->at_end_pair = false;
  gc_push_root(original);
  list_builder_init(&m->copy_builder);
}

bool list_mapper_finished(struct list_mapper *m) { return m->cursor == NULL; }
struct lisp_val list_mapper_get_next(struct list_mapper *m) {
  return m->at_end_pair ? m->cursor->cdr : m->cursor->car;
}

static void list_mapper_do_copy(struct list_mapper *m) {
  m->copied = true;
  struct lisp_val orig = m->original;
  while (lisp_val_type(orig) == LISP_CONS) {
    struct lisp_cons *cell = lisp_val_as_obj(orig);
    if (cell == m->cursor) {
      break;
    }
    list_builder_append(&m->copy_builder, cell->car);
    orig = cell->cdr;
  }
}

static void list_mapper_append_or_end_pair(struct list_mapper *m,
                                           struct lisp_val next) {
  if (m->at_end_pair) {
    list_builder_end_pair(&m->copy_builder, next);
  } else {
    list_builder_append(&m->copy_builder, next);
  }
}

void list_mapper_append_next(struct list_mapper *m, struct lisp_val next) {
  if (m->copied) {
    list_mapper_append_or_end_pair(m, next);
  } else if (!lisp_val_identical(list_mapper_get_next(m), next)) {
    // TODO deep equality instead?
    // Copy up to existing point
    gc_push_root(next);
    list_mapper_do_copy(m);
    gc_pop_root_expect(next);
    list_mapper_append_or_end_pair(m, next);
  }

  // Always advance
  if (m->at_end_pair) {
    m->cursor = NULL;
  } else if (lisp_val_type(m->cursor->cdr) == LISP_CONS) {
    m->cursor = lisp_val_as_obj(m->cursor->cdr);
  } else if (lisp_val_is_nil(m->cursor->cdr)) {
    m->cursor = NULL;
  } else {
    m->at_end_pair = true;
  }
}

struct lisp_val list_mapper_build(struct list_mapper *m) {
  // Call list_build to clean up it's state regardless if used
  struct lisp_val copy = list_build(&m->copy_builder);
  gc_pop_root_expect(m->original);
  if (m->copied) {
    return copy;
  } else {
    return m->original;
  }
}

struct lisp_hash_table_entry {
  struct lisp_obj header;
  struct lisp_hash_table_entry *next;
  struct lisp_symbol *key;
};

static void lisp_hash_table_entry_visit(struct lisp_val v, visit_callback cb,
                                        void *ctx) {
  const struct lisp_hash_table_entry *e = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(e->key));
}

static const struct lisp_vtable HASH_TABLE_ENTRY_VTABLE = {
    .type = LISP_INVALID,
    .is_gc_managed = true,
    .name = "hash-table-entry",
    .visit_children = lisp_hash_table_entry_visit,
    .destroy = destroy_none,
};

struct lisp_symbol_table {
  struct lisp_obj header;
  size_t capacity;
  size_t size;
  struct lisp_hash_table_entry *entries[];
};

struct lisp_symbol_map {
  struct lisp_symbol_table *table;
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
    .type = LISP_INVALID,
    .is_gc_managed = true,
    .name = "symbol_table",
    .visit_children = lisp_symbol_table_visit,
    // TODO Make the symbol table separately allocate its hash table entries?
    .destroy = destroy_none,
};

static struct lisp_symbol_table *lisp_symbol_table_create(size_t capacity) {
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
static struct lisp_hash_table_entry *symbol_table_lookup_symbol(
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
static struct lisp_symbol_table *lisp_symbl_table_insert(
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

#define SYMBOL_INTEN_INIT_CAP 32
struct lisp_symbol_table *symbol_intern_table = NULL;

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

  symbol_intern_table = lisp_symbl_table_insert(symbol_intern_table, entry);
  return entry->key;
}

void lisp_symbol_table_delete_if(struct lisp_symbol_table *m,
                                 bool (*pred)(struct lisp_symbol *sym)) {
  if (m == NULL) {
    return;
  }

  for (unsigned i = 0; i < m->capacity; i++) {
    struct lisp_hash_table_entry **entry = &m->entries[i];
    while (*entry != NULL) {
      if (pred((*entry)->key)) {
        // Replace the pointer in the linked list
        *entry = (*entry)->next;
        m->size--;
      } else {
        // Just advance the local variable
        entry = &(*entry)->next;
      }
    }
  }
}

// TODO Change to 1 in GC_DEBUG mode to trigger reallocations more easily
#define ENV_MAP_INIT_CAP 8

struct lisp_env {
  struct lisp_obj header;
  struct lisp_symbol_table *mappings;
  struct lisp_env *outer;
};

static void lisp_env_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  const struct lisp_env *env = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(env->mappings));
  if (env->outer != NULL) {
    cb(ctx, lisp_val_from_obj(env->outer));
  }
}

static const struct lisp_vtable ENV_VTABLE = {
    .type = LISP_INVALID,
    .is_gc_managed = true,
    .name = "environment",
    .visit_children = lisp_env_visit,
    .destroy = destroy_none,
};

struct lisp_env_table_entry {
  struct lisp_hash_table_entry ht;
  struct lisp_env_binding binding;
};

static void lisp_env_table_entry_visit(struct lisp_val v, visit_callback cb,
                                       void *ctx) {
  const struct lisp_env_table_entry *e = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(e->ht.key));
  cb(ctx, e->binding.value);
}

static const struct lisp_vtable ENV_TABLE_ENTRY_VTABLE = {
    .type = LISP_INVALID,
    .is_gc_managed = true,
    .name = "environment-entry",
    .visit_children = lisp_env_table_entry_visit,
    .destroy = destroy_none,
};

struct lisp_env *lisp_env_create(struct lisp_env *outer) {
  // TODO Is this necessary? Can it always be assumed that the outer
  // environment is already in the stack?
  gc_push_root_obj(outer);

  // Initialize the map before allocating the environment to ensure everything
  // is in a valid state immediately after allocation

  struct lisp_symbol_table *mappings =
      lisp_symbol_table_create(ENV_MAP_INIT_CAP);
  gc_push_root_obj(mappings);

  struct lisp_env *env = lisp_obj_alloc(&ENV_VTABLE, sizeof(*env));
  env->mappings = mappings;
  env->outer = outer;

  gc_pop_root_expect_obj(mappings);
  gc_pop_root_expect_obj(outer);

  return env;
}

const struct lisp_env_binding *lisp_env_get(const struct lisp_env *env,
                                            struct lisp_symbol *sym) {
  struct lisp_env_table_entry *existing =
      (struct lisp_env_table_entry *)symbol_table_lookup_entry(
          env->mappings, lisp_symbol_name(sym), lisp_symbol_length(sym));
  if (existing == NULL) {
    if (env->outer != NULL) {
      return lisp_env_get(env->outer, sym);
    } else {
      return false;
    }
  } else {
    return &existing->binding;
  }
}

static void lisp_env_set_or_insert(struct lisp_env *env,
                                   struct lisp_symbol *sym,
                                   struct lisp_env_binding binding) {
  struct lisp_env_table_entry *entry =
      (struct lisp_env_table_entry *)symbol_table_lookup_symbol(env->mappings,
                                                                sym);
  if (entry != NULL) {
    entry->binding = binding;
  }

  gc_push_root_obj(sym);
  gc_push_root(binding.value);

  entry = lisp_obj_alloc(&ENV_TABLE_ENTRY_VTABLE, sizeof(*entry));
  entry->ht.key = sym;
  entry->binding = binding;

  gc_pop_root_expect(binding.value);
  gc_pop_root_expect_obj(sym);

  env->mappings = lisp_symbl_table_insert(
      env->mappings, (struct lisp_hash_table_entry *)entry);
}

void lisp_env_set(struct lisp_env *env, struct lisp_symbol *sym,
                  struct lisp_val val) {
  lisp_env_set_or_insert(env, sym,
                         (struct lisp_env_binding){
                             .value = val,
                             .is_macro = false,
                         });
}

void lisp_env_set_macro(struct lisp_env *env, struct lisp_symbol *sym,
                        struct lisp_val val) {
  lisp_env_set_or_insert(env, sym,
                         (struct lisp_env_binding){
                             .value = val,
                             .is_macro = true,
                         });
}

struct lisp_closure {
  struct lisp_obj header;
  struct lisp_symbol *name;
  unsigned arg_count;
  bool is_variadic;
  struct lisp_env *outer_env;
  struct code_chunk *code;
};

static void lisp_closure_visit(struct lisp_val v, visit_callback cb,
                               void *ctx) {
  const struct lisp_closure *cl = lisp_val_as_obj(v);
  if (cl->name != NULL) {
    cb(ctx, lisp_val_from_obj(cl->name));
  }
  cb(ctx, lisp_val_from_obj(cl->outer_env));
  cb(ctx, lisp_val_from_obj(cl->code));
}

static const struct lisp_vtable CLOSURE_VTABLE = {
    .type = LISP_CLOSURE,
    .is_gc_managed = true,
    .name = "closure",
    .visit_children = lisp_closure_visit,
    .destroy = destroy_none,
};

struct lisp_closure *lisp_closure_create(unsigned arg_count, bool is_variadic,
                                         struct lisp_env *outer_env,
                                         struct code_chunk *bytecode) {
  gc_push_root_obj(outer_env);
  gc_push_root_obj(bytecode);

  struct lisp_closure *cl = lisp_obj_alloc(&CLOSURE_VTABLE, sizeof(*cl));
  cl->name = NULL;
  cl->arg_count = arg_count;
  cl->is_variadic = is_variadic;
  cl->outer_env = outer_env;
  cl->code = bytecode;

  gc_pop_root_expect_obj(bytecode);
  gc_pop_root_expect_obj(outer_env);
  return cl;
}

struct lisp_symbol *lisp_closure_name(const struct lisp_closure *c) {
  return c->name;
}

const char *lisp_closure_name_cstr(const struct lisp_closure *c) {
  return c->name != NULL ? lisp_symbol_name(c->name) : "#<unknown>";
}

unsigned lisp_closure_arg_count(const struct lisp_closure *c) {
  return c->arg_count;
}

bool lisp_closure_is_variadic(const struct lisp_closure *c) {
  return c->is_variadic;
}

struct lisp_env *lisp_closure_env(const struct lisp_closure *c) {
  return c->outer_env;
}

struct code_chunk *lisp_closure_code(const struct lisp_closure *c) {
  return c->code;
}

// TODO Force setting at construction time?
void lisp_closure_set_name(struct lisp_closure *c, struct lisp_symbol *name) {
  c->name = name;
}

static const struct lisp_vtable *const TYPE_TO_VTABLE[] = {
    [LISP_NIL] = &NIL_VTABLE,       [LISP_INT] = &INT_VTABLE,
    [LISP_REAL] = &REAL_VTABLE,     [LISP_CHAR] = &CHAR_VTABLE,
    [LISP_STRING] = &STRING_VTABLE, [LISP_SYMBOL] = &SYMBOL_VTABLE,
    [LISP_CONS] = &CONS_VTABLE,     [LISP_CLOSURE] = &CLOSURE_VTABLE,
    [LISP_ATOM] = &ATOM_VTABLE,
};

static_assert(sizeof(TYPE_TO_VTABLE) / sizeof(void *) == LISP_TYPE_MAX - 1,
              "missing vtable entry");

const char *lisp_type_name(enum lisp_type t) {
  if (t == LISP_OPAQUE) {
    return "opaque";
  } else if (LISP_TYPE_MIN <= t && t < LISP_TYPE_MAX) {
    return TYPE_TO_VTABLE[t]->name;
  } else {
    return "unknown";
  }
}
