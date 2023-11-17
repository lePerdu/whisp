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
  TAG_OBJ = 0,
  TAG_INT = 1,
  TAG_CHAR = 2,
};

#define INT_TAG_SHIFT 1
#define INT_TAG_MASK ((uintptr_t)1)
// Store characters in the upper 32-bit word as that can probably be more
// optimized by the compiler.
#define CHAR_TAG_SHIFT 32
#define OBJ_TAG_MASK ((uintptr_t)3)
#define OBJ_TAG_SHIFT 2

const char *lisp_val_type_name(struct lisp_val v);

bool lisp_val_identical(struct lisp_val a, struct lisp_val b);

typedef void (*visit_callback)(void *ctx, struct lisp_val v);

enum lisp_obj_alloc_type {
  /**
   * Statically allocated contant object. These objects must not contain
   * references to other objects.
   */
  LISP_ALLOC_CONST,

  /**
   * Heap-allocated, GC-managed object.
   */
  LISP_ALLOC_GC,

  /**
   * Stack-allocated, GC-aware object.
   *
   * These objects can contain references to GC-allocated objects, but must not
   * be referenced from the heap.
   */
  LISP_ALLOC_STACK,
};

struct lisp_vtable {
  const char *name;

  // TODO Should this be an object field instead? (Probably not worth adding an
  // extra flag to every object)
  enum lisp_obj_alloc_type alloc_type;

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
 * No-op visit callback.
 */
void lisp_visit_none(struct lisp_val object, visit_callback cb, void *ctx);

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
  assert((v.tagged_ptr & OBJ_TAG_MASK) == TAG_OBJ);
  return (void *)v.tagged_ptr;
}

static inline struct lisp_val lisp_val_from_obj(void *obj) {
  uintptr_t raw = (uintptr_t)obj;
  assert((raw & OBJ_TAG_MASK) == TAG_OBJ);
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
    lisp_int_t: "int", \
    lisp_char_t: "char", \
    lisp_real_t: "real", \
    struct lisp_symbol *: "symbol", \
    struct lisp_string *: "string", \
    struct lisp_cons *: "cons", \
    struct lisp_atom *: "atom", \
    struct lisp_array *: "array", \
    struct lisp_closure *: "function", \
    struct str_builder *: "string-builder", \
    struct lisp_file_port *: "port" \
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
 * Special object to mark uninitialized value.
 */
struct lisp_val lisp_uninitialized(void);
bool lisp_is_uninitialized(struct lisp_val v);

typedef long lisp_int_t;

/**
 * Unsigned integer type is used in a couple scenarios.
 */
typedef unsigned long lisp_unsigned_int_t;

#define LISP_INT_MIN (LONG_MIN >> 2)
#define LISP_INT_MAX (LONG_MAX >> 2)

/** Fixed-sized integer. */
static inline struct lisp_val lisp_val_from_int(lisp_int_t n) {
  // Having the assertions aborts the program on integer overflow.
  // TODO Make a runtime error? Just ignore overflow?
  // assert(n >= LISP_INT_MIN);
  // assert(n <= LISP_INT_MAX);
  return (struct lisp_val){(uintptr_t)(n << INT_TAG_SHIFT) + TAG_INT};
}

static inline bool lisp_val_is_int(struct lisp_val v) {
  return (v.tagged_ptr & INT_TAG_MASK) == TAG_INT;
}

static inline lisp_int_t lisp_val_as_int(struct lisp_val v) {
  assert(lisp_val_is_int(v));
  return ((lisp_int_t)v.tagged_ptr) >> INT_TAG_SHIFT;
}

typedef unsigned char lisp_char_t;

#define LISP_CHAR_MIN 0
#define LISP_CHAR_MAX UINT8_MAX

/** Single character (only supports 8-bit for now) */
static inline struct lisp_val lisp_val_from_char(lisp_char_t c) {
  return (struct lisp_val){((uintptr_t)c << CHAR_TAG_SHIFT) + TAG_CHAR};
}

static inline bool lisp_val_is_char(struct lisp_val v) {
  return (v.tagged_ptr & OBJ_TAG_MASK) == TAG_CHAR;
}

static inline lisp_char_t lisp_val_as_char(struct lisp_val v) {
  assert(lisp_val_is_char(v));
  return (lisp_char_t)(v.tagged_ptr >> CHAR_TAG_SHIFT);
}

typedef double lisp_real_t;

/** Double-precision floating point number. */
struct lisp_val lisp_val_from_real(lisp_real_t d);
bool lisp_val_is_real(struct lisp_val v);
lisp_real_t lisp_val_as_real(struct lisp_val v);

bool lisp_val_is_number(struct lisp_val v);

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

/** Immutable array of characters. */
struct lisp_string {
  struct lisp_obj header;
  size_t length;
  char data[];
};

struct lisp_string *lisp_string_create(const char *s, size_t len);
struct lisp_string *lisp_string_create_cstr(const char *s);
struct lisp_string *lisp_string_create_empty(size_t len);
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
  struct lisp_obj header;
  struct lisp_string *buf;
  size_t capacity;
};

/**
 * Stack-allocated string builder.
 *
 * The managed data is GC-preserved until `str_build`.
 */
void str_builder_init(struct str_builder *b);
void str_builder_init_cap(struct str_builder *b, size_t capacity);

/**
 * Heap-allocated string builder.
 */
struct str_builder *str_builder_create();
bool lisp_val_is_str_builder(struct lisp_val v);

void str_builder_append(struct str_builder *b, char c);
void str_builder_concat(struct str_builder *b, const struct lisp_string *s);
void str_builder_concat_cstr(struct str_builder *b, const char *cstr);
void str_builder_concat_n(struct str_builder *b, const char *buf, size_t n);

// Functions for low-level inspection/manipulation

void str_builder_ensure_cap(struct str_builder *b, size_t added_size);
size_t str_builder_remaining_cap(struct str_builder *b);
void str_builder_include_size(struct str_builder *b, size_t added_size);

char *str_builder_raw_buf(struct str_builder *b);
char *str_builder_raw_buf_end(struct str_builder *b);
const struct lisp_string *str_builder_get_str(struct str_builder *b);

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
  struct lisp_obj header;
  // Store a lisp_val since the list is initially nil
  struct lisp_val head;
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
