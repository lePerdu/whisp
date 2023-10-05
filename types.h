#ifndef TYPES_H_
#define TYPES_H_

#include <assert.h>
#include <limits.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// TODO Get rid of type enum in favor of vtables?
enum lisp_type {
  /** Singleton nil type. */
  LISP_NIL,
  /** Fixed-sized integer. */
  LISP_INT,
  /** Double-precision floating point number. */
  LISP_REAL,
  /** Single character (only supports 8-bit for now) */
  LISP_CHAR,
  /**
   * Symbol used for all identifiers.
   * These are always interned so that comparison is just a pointer comparison.
   */
  LISP_SYMBOL,
  /** Immutable array of characters. */
  LISP_STRING,
  /** Pair of items. Used to represent lists as well. */
  LISP_CONS,
  /**
   * Mutable cell for storing a single value.
   *
   * TODO Implement atom in lisp since it's basically just an array of length 1?
   */
  LISP_ATOM,
  /**
   * Fixed-sized, mutable array of lisp values.
   *
   * This isn't intended to be used directly very often except as an
   * optimization. It is intended to be wrapped in safer structures such as
   * records or persistent vectors.
   */
  LISP_ARRAY,

  /** Lisp-defined function. */
  LISP_CLOSURE,

  /**
   * Opaque, bulitin object.
   *
   * This exists as a well-defined alternative to LISP_INVALID when an object is
   * intended to be available to LISP code.
   *
   * TODO Better name?
   * TODO Merge with INVALID?
   */
  LISP_OPAQUE,

  /**
   * Type to mark objects internal to the runtime, not accessible from LISP.
   */
  LISP_INVALID,
};

#define LISP_TYPE_MIN LISP_NIL
#define LISP_TYPE_MAX LISP_INVALID

struct lisp_val {
  uintptr_t tagged_ptr;
};

enum lisp_type lisp_val_type(struct lisp_val v);

// TODO Remove one of these
const char *lisp_val_type_name(struct lisp_val v);
const char *lisp_type_name(enum lisp_type t);

bool lisp_val_identical(struct lisp_val a, struct lisp_val b);

typedef void (*visit_callback)(void *ctx, struct lisp_val v);

extern const struct lisp_val LISP_VAL_NIL;

struct lisp_vtable {
  enum lisp_type type;
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

struct lisp_val lisp_val_from_obj(void *obj);
void *lisp_val_as_obj(struct lisp_val v);

/**
 * Check the type of a lisp_val and convert to an object pointer.
 * If the type does not match, returns NULL.
 */
void *lisp_val_cast(enum lisp_type type, struct lisp_val v);

#define LISP_VAL_AS(t, v) ((t *)lisp_val_as_obj(v))

// clang-format doesn't handle _Generic macros well
// clang-format off
#define LISP_CONVERT_C_TYPE(t) \
  _Generic((t *)NULL, \
    long *: LISP_INT, \
    lisp_char_t *: LISP_CHAR, \
    double *: LISP_REAL, \
    struct lisp_symbol *: LISP_SYMBOL, \
    struct lisp_string *: LISP_STRING, \
    struct lisp_cons *: LISP_CONS, \
    struct lisp_atom *: LISP_ATOM, \
    struct lisp_array *: LISP_ARRAY, \
    struct lisp_closure *: LISP_CLOSURE, \
  )
// clang-format on

/*
 * Macro version of lisp_val_cast which takes in the C type rather than the enum
 * value for a bit more type safety.
 *
#define LISP_CAST(t, v) ((t *)lisp_val_cast(LISP_CONVERT_C_TYPE(t), v))
*/

bool lisp_val_is_nil(struct lisp_val v);

// Cannonical true and false values
struct lisp_val lisp_true(void);
struct lisp_val lisp_false(void);
bool lisp_val_is_true(struct lisp_val v);
bool lisp_val_is_false(struct lisp_val v);

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

bool lisp_val_is_number(struct lisp_val v);
/**
 * Returns whether a value is a "proper" list. I.e. nil or a series of cons
 * cells which end in nil.
 */
bool lisp_val_is_list(struct lisp_val v);
bool lisp_val_is_func(struct lisp_val v);

#define LISP_INT_MIN (LONG_MIN >> 2)
#define LISP_INT_MAX (LONG_MAX >> 2)

struct lisp_val lisp_val_from_int(long n);
long lisp_val_as_int(struct lisp_val v);

struct lisp_val lisp_val_from_real(double d);
double lisp_val_as_real(struct lisp_val v);

typedef unsigned char lisp_char_t;

#define LISP_CHAR_MIN 0
#define LISP_CHAR_MAX UINT8_MAX

struct lisp_val lisp_val_from_char(lisp_char_t c);
lisp_char_t lisp_val_as_char(struct lisp_val v);

/**
 * Mutable cell which can contain any value.
 */
struct lisp_atom;

struct lisp_atom *lisp_atom_create(struct lisp_val v);
struct lisp_val lisp_atom_deref(const struct lisp_atom *a);
void lisp_atom_reset(struct lisp_atom *a, struct lisp_val v);
bool lisp_atom_eq(const struct lisp_atom *a, const struct lisp_atom *b);

struct lisp_array;

/**
 * Create a new array filled with nil values.
 */
struct lisp_array *lisp_array_create(size_t length);

size_t lisp_array_length(const struct lisp_array *arr);
struct lisp_val lisp_array_get(const struct lisp_array *arr, size_t index);
void lisp_array_set(struct lisp_array *arr, size_t index, struct lisp_val new);

struct lisp_symbol;

struct lisp_symbol *lisp_symbol_create(const char *s, size_t len);
/**
 * Create symbol from null-terminated string.
 *
 * The contents of the string are not checked for invalid characters/format,
 * so checking should be done up-front.
 */
struct lisp_symbol *lisp_symbol_create_cstr(const char *s);
const char *lisp_symbol_name(const struct lisp_symbol *s);
size_t lisp_symbol_length(const struct lisp_symbol *s);
bool lisp_symbol_eq(const struct lisp_symbol *a, const struct lisp_symbol *b);

extern struct lisp_symbol_table *symbol_intern_table;

// TODO This is very much a mix of concerns going on...
void lisp_symbol_table_delete_if(struct lisp_symbol_table *map,
                                 bool (*pred)(struct lisp_symbol *sym));

struct lisp_string;

struct lisp_string *lisp_string_create(const char *s, size_t len);
struct lisp_string *lisp_string_create_cstr(const char *s);

/**
 * Create a lisp_string using a printf-compatible format string.
 */
struct lisp_string *lisp_string_format(const char *format, ...);
struct lisp_string *lisp_string_vformat(const char *format, va_list ap);

size_t lisp_string_length(const struct lisp_string *s);
char lisp_string_get(const struct lisp_string *s, size_t i);
const char *lisp_string_as_cstr(const struct lisp_string *s);
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

struct lisp_cons {
  struct lisp_obj header;
  struct lisp_val car;
  struct lisp_val cdr;
};

struct lisp_cons *lisp_cons_create(struct lisp_val car, struct lisp_val cdr);
bool lisp_cons_eq(const struct lisp_cons *a, const struct lisp_cons *b);

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

struct lisp_env_binding {
  struct lisp_val value;
  bool is_macro;
};

/**
 * Key-value mapping used for the global environment.
 */
struct lisp_env;

struct lisp_env *lisp_env_create(void);

/**
 * Lookup a value in the environment. Returns NULL (not LISP_VAL_NIL) if the
 * symbol is not mapped.
 */
const struct lisp_env_binding *lisp_env_get(const struct lisp_env *env,
                                            struct lisp_symbol *sym);

// Mutate the environment. If the value cannot be set (i.e. if tring to set
// a constant), returns false

void lisp_env_set(struct lisp_env *env, struct lisp_symbol *sym,
                  struct lisp_val val);
void lisp_env_set_macro(struct lisp_env *env, struct lisp_symbol *sym,
                        struct lisp_val val);

struct lisp_closure;
struct code_chunk;

struct lisp_closure *lisp_closure_create(struct code_chunk *bytecode,
                                         size_t n_captures);
struct lisp_symbol *lisp_closure_name(const struct lisp_closure *c);
const char *lisp_closure_name_cstr(const struct lisp_closure *c);
struct code_chunk *lisp_closure_code(const struct lisp_closure *c);
unsigned lisp_closure_arg_count(const struct lisp_closure *c);
bool lisp_closure_is_variadic(const struct lisp_closure *c);

size_t lisp_closure_n_captures(const struct lisp_closure *c);
struct lisp_val lisp_closure_get_capture(const struct lisp_closure *c,
                                         unsigned index);
void lisp_closure_set_capture(struct lisp_closure *c, unsigned index,
                              struct lisp_val val);

#endif
