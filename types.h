#ifndef TYPES_H_
#define TYPES_H_

#include <assert.h>
#include <limits.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct lisp_val {
  uintptr_t tagged_ptr;
};

enum ptr_tag {
  TAG_INT = 1,
  TAG_OBJ = 0,
};

#define TAG_SHIFT 1
#define TAG_MASK ((uintptr_t)1)
#define VALUE_MASK (~TAG_MASK)

const char *lisp_val_type_name(struct lisp_val v);

bool lisp_val_identical(struct lisp_val a, struct lisp_val b);

typedef void (*visit_callback)(void *ctx, struct lisp_val v);

struct lisp_vtable {
  const char *name;

  // TODO Should this be an object field instead? (Probably not worth adding an
  // extra flag to every object)
  bool is_gc_managed;

  /**
   * Call the visitor on direct children (should not be recursive).
   */
  void (*visit_children)(struct lisp_val v, visit_callback callback, void *ctx);

  /**
   * Destroy the object by freeing any non-GC managed resources.
   *
   * This should NOT free the object itself; that will be handled by the garbage
   * collector.
   */
  void (*destroy)(struct lisp_val v);
};

/**
 * No-op destroy callback.
 */
void lisp_destroy_none(struct lisp_val v);

/**
 * Metadata stored for every object.
 */
struct lisp_obj {
  const struct lisp_vtable *vt;
};

void *lisp_obj_alloc(const struct lisp_vtable *vt, size_t size);

/**
 * Return the object vtable. This works even for primitive, non-GC managed
 * types.
 */
const struct lisp_vtable *lisp_val_vtable(struct lisp_val v);

typedef bool (*lisp_predicate)(struct lisp_val arg);

// Since the object tag is 0, pointer conversions can be a no-op
static_assert(TAG_OBJ == 0, "Object tag is non-0");

static inline void *lisp_val_as_obj(struct lisp_val v) {
  assert((v.tagged_ptr & TAG_MASK) == TAG_OBJ);
  return (void *)v.tagged_ptr;
}

static inline struct lisp_val lisp_val_from_obj(void *obj) {
  uintptr_t raw = (uintptr_t)obj;
  assert((raw & TAG_MASK) == 0);
  return (struct lisp_val){raw};
}

/**
 * Check the type of a lisp_val and convert to an object pointer.
 * If the type does not match, returns NULL.
 */
static inline void *lisp_val_cast(lisp_predicate pred, struct lisp_val v) {
  return pred(v) ? lisp_val_as_obj(v) : NULL;
}

#define LISP_VAL_AS(t, v) ((t *)lisp_val_as_obj(v))

// clang-format doesn't handle _Generic macros well
// clang-format off
#define LISP_TYPE_NAME(t) \
  _Generic((t)(uintptr_t)NULL, \
    long: "int", \
    lisp_char_t: "char", \
    double: "real", \
    struct lisp_symbol *: "symbol", \
    struct lisp_string *: "string", \
    struct lisp_cons *: "cons", \
    struct lisp_atom *: "atom", \
    struct lisp_array *: "array", \
    struct lisp_closure *: "function" \
  )
// clang-format on

/*
 * Macro version of lisp_val_cast which takes in the C type rather than the enum
 * value for a bit more type safety.
 *
#define LISP_CAST(t, v) ((t *)lisp_val_cast(LISP_CONVERT_C_TYPE(t), v))
*/

/** Singleton nil object. */
extern const struct lisp_val LISP_VAL_NIL;

static inline bool lisp_val_is_nil(struct lisp_val v) {
  return v.tagged_ptr == LISP_VAL_NIL.tagged_ptr;
}

// Cannonical true and false values
struct lisp_val lisp_true(void);
struct lisp_val lisp_false(void);

static inline bool lisp_val_is_false(struct lisp_val v) {
  return lisp_val_is_nil(v);
}

static inline bool lisp_val_is_true(struct lisp_val v) {
  return !lisp_val_is_false(v);
}

static inline struct lisp_val lisp_val_from_bool(bool x) {
  return x ? lisp_true() : lisp_false();
}

/**
 * Object returned by forms of unspecified return to indicate that the REPL
 * should not print their result.
 *
 * E.g. `def!`, `display`
 */
struct lisp_val lisp_non_printing(void);
bool lisp_is_non_printing(struct lisp_val v);

/**
 * Special object to mark uninitialized value.
 */
struct lisp_val lisp_uninitialized(void);
bool lisp_is_uninitialized(struct lisp_val v);

#define LISP_INT_MIN (LONG_MIN >> 2)
#define LISP_INT_MAX (LONG_MAX >> 2)

/** Fixed-sized integer. */
static inline struct lisp_val lisp_val_from_int(long n) {
  // Having the assertions aborts the program on integer overflow.
  // TODO Make a runtime error? Just ignore overflow?
  // assert(n >= LISP_INT_MIN);
  // assert(n <= LISP_INT_MAX);
  return (struct lisp_val){(uintptr_t)(n << TAG_SHIFT) | TAG_INT};
}

static inline bool lisp_val_is_int(struct lisp_val v) {
  return (v.tagged_ptr & TAG_MASK) == TAG_INT;
}

static inline long lisp_val_as_int(struct lisp_val v) {
  assert(lisp_val_is_int(v));
  return ((long)v.tagged_ptr) >> TAG_SHIFT;
}

/** Double-precision floating point number. */
struct lisp_val lisp_val_from_real(double d);
bool lisp_val_is_real(struct lisp_val v);
double lisp_val_as_real(struct lisp_val v);

bool lisp_val_is_number(struct lisp_val v);

typedef unsigned char lisp_char_t;

#define LISP_CHAR_MIN 0
#define LISP_CHAR_MAX UINT8_MAX

/** Single character (only supports 8-bit for now) */
struct lisp_val lisp_val_from_char(lisp_char_t c);
bool lisp_val_is_char(struct lisp_val v);
lisp_char_t lisp_val_as_char(struct lisp_val v);

/**
 * Mutable cell for storing a single value.
 *
 * TODO Implement atom in lisp since it's basically just an array of length 1?
 */
struct lisp_atom {
  struct lisp_obj header;
  struct lisp_val value;
};

struct lisp_atom *lisp_atom_create(struct lisp_val v);
bool lisp_val_is_atom(struct lisp_val v);

static inline struct lisp_val lisp_atom_deref(const struct lisp_atom *a) {
  return a->value;
}
static inline void lisp_atom_reset(struct lisp_atom *a, struct lisp_val v) {
  a->value = v;
}

/**
 * Fixed-sized, mutable array of lisp values.
 *
 * This isn't intended to be used directly very often except as an
 * optimization. It is intended to be wrapped in safer structures such as
 * records or persistent vectors.
 */
struct lisp_array {
  struct lisp_obj header;
  size_t length;
  struct lisp_val data[];
};

/**
 * Create a new array filled with nil values.
 */
struct lisp_array *lisp_array_create(size_t length);
bool lisp_val_is_array(struct lisp_val v);

size_t lisp_array_length(const struct lisp_array *arr);

static inline struct lisp_val lisp_array_get(const struct lisp_array *arr,
                                             size_t index) {
  assert(index < arr->length);
  return arr->data[index];
}

static inline void lisp_array_set(struct lisp_array *arr, size_t index,
                                  struct lisp_val new) {
  assert(index < arr->length);
  arr->data[index] = new;
}

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

extern struct lisp_symbol_table *symbol_intern_table;

// TODO This is very much a mix of concerns going on...
void lisp_symbol_table_delete_if(struct lisp_symbol_table *map,
                                 bool (*pred)(struct lisp_symbol *sym));

/** Immutable array of characters. */
struct lisp_string {
  struct lisp_obj header;
  size_t length;
  char data[];
};

struct lisp_string *lisp_string_create(const char *s, size_t len);
struct lisp_string *lisp_string_create_cstr(const char *s);
bool lisp_val_is_string(struct lisp_val v);

/**
 * Create a lisp_string using a printf-compatible format string.
 */
struct lisp_string *lisp_string_format(const char *format, ...);
struct lisp_string *lisp_string_vformat(const char *format, va_list ap);

static inline char lisp_string_get(const struct lisp_string *s, size_t i) {
  // <= to allow fetching the null byte
  assert(i <= s->length);
  return s->data[i];
}

static inline const char *lisp_string_as_cstr(const struct lisp_string *s) {
  return s->data;
}

bool lisp_string_eq(const struct lisp_string *a, const struct lisp_string *b);

struct str_builder {
  struct lisp_atom *str_atom;
  size_t capacity;
};

void str_builder_init(struct str_builder *b);
void str_builder_init_cap(struct str_builder *b, size_t capacity);
const struct lisp_string *str_builder_get_str(const struct str_builder *b);
void str_builder_append(struct str_builder *b, char c);
void str_builder_concat(struct str_builder *b, const struct lisp_string *s);
void str_builder_concat_cstr(struct str_builder *b, const char *cstr);
void str_builder_concat_n(struct str_builder *b, const char *buf, size_t n);

/**
 * Append a printf-like format string.
 *
 * Returns the same as snprintf.
 */
int str_builder_format(struct str_builder *b, const char *format, ...);
int str_builder_vformat(struct str_builder *b, const char *format, va_list ap);

struct lisp_string *str_build(struct str_builder *b);

/** Pair of items. Used to represent lists as well. */
struct lisp_cons {
  struct lisp_obj header;
  struct lisp_val car;
  struct lisp_val cdr;
};

struct lisp_cons *lisp_cons_create(struct lisp_val car, struct lisp_val cdr);
bool lisp_val_is_cons(struct lisp_val v);
/**
 * Returns whether a value is a "proper" list. I.e. nil or a series of cons
 * cells which end in nil.
 */
bool lisp_val_is_list(struct lisp_val v);

unsigned lisp_list_count(struct lisp_val list);

/**
 * Builds up a lisp list (or pair) incrementally.
 *
 * The list is marked as a GC root while building, so list_build() MUST be
 * called befor the builder goes out of scope.
 */
struct list_builder {
  // Store an atom since the list is initially nil, but then becomes the
  // head
  // TODO Avoid GC'd atom here
  struct lisp_atom *list_atom;
  struct lisp_cons *tail;
};

void list_builder_init(struct list_builder *b);

void list_builder_append(struct list_builder *b, struct lisp_val v);

/**
 * Terminate the list with a non-nil value.
 *
 * Conditions:
 * - The list must not be empty.
 * - This should be the last element added to the list.
 */
void list_builder_end_pair(struct list_builder *b, struct lisp_val pair_end);

struct lisp_val list_builder_current(const struct list_builder *b);

/**
 * Return the built list. (Note: this will return NIL if the list is empty)
 */
struct lisp_val list_build(struct list_builder *b);

/**
 * Utility for "mapping" over lists with the optimization that an identity
 * mapping will avoid re-creating the list (a sort of copy-on-modify).
 */
struct list_mapper {
  struct lisp_val original;
  struct lisp_cons *cursor;
  bool copied;
  bool at_end_pair;
  struct list_builder copy_builder;
};

void list_mapper_init(struct list_mapper *m, struct lisp_val original);
bool list_mapper_finished(struct list_mapper *m);
struct lisp_val list_mapper_get_next(struct list_mapper *m);
void list_mapper_append_next(struct list_mapper *m, struct lisp_val next);

/**
 * Return the built list. (Note: this will return NIL if the list is empty)
 */
struct lisp_val list_mapper_build(struct list_mapper *m);

/**
 * Key-value mapping used for the global environment.
 */
struct lisp_env;
struct lisp_env_binding;

struct lisp_env *lisp_env_create(void);
bool lisp_val_is_env(struct lisp_val v);

/**
 * Lookup a value in the environment. Returns NULL (not LISP_VAL_NIL) if the
 * symbol is not mapped.
 */
struct lisp_env_binding *lisp_env_get(struct lisp_env *env,
                                      struct lisp_symbol *sym);

struct lisp_env_binding *lisp_env_set(struct lisp_env *env,
                                      struct lisp_symbol *sym,
                                      struct lisp_val val);
struct lisp_env_binding *lisp_env_set_macro(struct lisp_env *env,
                                            struct lisp_symbol *sym,
                                            struct lisp_val val);

bool lisp_val_is_env_binding(struct lisp_val v);
const struct lisp_symbol *lisp_env_binding_name(
    const struct lisp_env_binding *b);
struct lisp_val lisp_env_binding_value(const struct lisp_env_binding *b);
void lisp_env_binding_set_value(struct lisp_env_binding *b,
                                struct lisp_val new);
bool lisp_env_binding_is_macro(const struct lisp_env_binding *b);

struct code_chunk;

/** Lisp-defined function. */
struct lisp_closure {
  struct lisp_obj header;
  struct code_chunk *code;
  size_t n_captures;
  struct lisp_val captures[];
};

struct lisp_closure *lisp_closure_create(struct code_chunk *bytecode,
                                         size_t n_captures);
bool lisp_val_is_func(struct lisp_val v);

struct lisp_symbol *lisp_closure_name(const struct lisp_closure *c);
const char *lisp_closure_name_cstr(const struct lisp_closure *c);

static inline struct lisp_val lisp_closure_get_capture(
    const struct lisp_closure *c, unsigned index) {
  assert(index < c->n_captures);
  return c->captures[index];
}

static inline void lisp_closure_set_capture(struct lisp_closure *c,
                                            unsigned index,
                                            struct lisp_val val) {
  assert(index < c->n_captures);
  c->captures[index] = val;
}

#endif
