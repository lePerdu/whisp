#include "types.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bytecode.h"
#include "memory.h"
#include "symbol.h"

#define REQUIRED_ALIGN (TAG_MASK + 1)

static_assert(alignof(struct lisp_obj) >= REQUIRED_ALIGN,
              "Alignment not big enough for tagged pointers");

void *lisp_obj_alloc(const struct lisp_vtable *vt, size_t size) {
  struct lisp_obj *obj = lisp_alloc(size);
  obj->vt = vt;
  return obj;
}

void lisp_visit_none(struct lisp_val object, visit_callback cb, void *ctx) {
  (void)object;
  (void)cb;
  (void)ctx;
}

void lisp_destroy_none(struct lisp_val object) { (void)object; }

static const struct lisp_vtable INT_VTABLE = {
    .alloc_type = LISP_ALLOC_CONST,
    .name = "int",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

static const struct lisp_vtable NIL_VTABLE = {
    .alloc_type = LISP_ALLOC_CONST,
    .name = "nil",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

const struct lisp_vtable *lisp_val_vtable(struct lisp_val v) {
  if ((v.tagged_ptr & TAG_MASK) == TAG_INT) {
    return &INT_VTABLE;
  } else if (v.tagged_ptr == LISP_VAL_NIL.tagged_ptr) {
    return &NIL_VTABLE;
  } else {
    return LISP_VAL_AS(struct lisp_obj, v)->vt;
  }
}

const char *lisp_val_type_name(struct lisp_val v) {
  return lisp_val_vtable(v)->name;
}

// TODO Make a pointer to an actual object? Would have to fix
// `lisp_val_from_obj` to account for C NULL and/or make sure NULL is never
// passed to it. = lisp_val_from_obj(NULL)
const struct lisp_val LISP_VAL_NIL = {TAG_OBJ};

static const struct lisp_vtable NON_PRINTING_VTABLE = {
    .alloc_type = LISP_ALLOC_CONST,
    .name = "non-printing",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

struct lisp_val lisp_non_printing(void) {
  static struct lisp_obj NON_PRINTING_VAL = {
      .vt = &NON_PRINTING_VTABLE,
  };

  return lisp_val_from_obj(&NON_PRINTING_VAL);
}

bool lisp_is_non_printing(struct lisp_val v) {
  return lisp_val_vtable(v) == &NON_PRINTING_VTABLE;
}

static const struct lisp_vtable UNINIT_VTABLE = {
    .name = "uninitialized",
    .alloc_type = LISP_ALLOC_CONST,
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

struct lisp_val lisp_uninitialized(void) {
  static struct lisp_obj UNINITIALIZED_VAL = {
      .vt = &UNINIT_VTABLE,
  };

  return lisp_val_from_obj(&UNINITIALIZED_VAL);
}

bool lisp_is_uninitialized(struct lisp_val v) {
  return lisp_val_vtable(v) == &UNINIT_VTABLE;
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

struct lisp_real {
  struct lisp_obj header;
  double value;
};

static const struct lisp_vtable REAL_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "real",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

struct lisp_val lisp_val_from_real(double d) {
  struct lisp_real *v = lisp_obj_alloc(&REAL_VTABLE, sizeof(*v));
  v->value = d;
  return lisp_val_from_obj(v);
}

bool lisp_val_is_real(struct lisp_val v) {
  return lisp_val_vtable(v) == &REAL_VTABLE;
}

double lisp_val_as_real(struct lisp_val v) {
  assert(lisp_val_is_real(v));
  return LISP_VAL_AS(struct lisp_real, v)->value;
}

bool lisp_val_is_number(struct lisp_val v) {
  return lisp_val_is_int(v) || lisp_val_is_real(v);
}

struct lisp_char {
  struct lisp_obj header;
  lisp_char_t value;
};

static const struct lisp_vtable CHAR_VTABLE = {
    // TODO Figure out a way to avoid heap allocating these?
    // For strictly 8-bit characters, they could be statically allocated, but
    // not when e.g. UTF-8 chars are supported
    .alloc_type = LISP_ALLOC_GC,
    .name = "char",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

struct lisp_val lisp_val_from_char(lisp_char_t c) {
  struct lisp_char *v = lisp_obj_alloc(&CHAR_VTABLE, sizeof(*v));
  v->value = c;
  return lisp_val_from_obj(v);
}

bool lisp_val_is_char(struct lisp_val v) {
  return lisp_val_vtable(v) == &CHAR_VTABLE;
}

lisp_char_t lisp_val_as_char(struct lisp_val v) {
  assert(lisp_val_is_char(v));
  return LISP_VAL_AS(struct lisp_char, v)->value;
}

bool lisp_val_identical(struct lisp_val a, struct lisp_val b) {
  return a.tagged_ptr == b.tagged_ptr;
}

static void lisp_atom_visit(struct lisp_val a, visit_callback cb, void *ctx) {
  cb(ctx, LISP_VAL_AS(struct lisp_atom, a)->value);
}

static const struct lisp_vtable ATOM_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "atom",
    .visit_children = lisp_atom_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_atom *lisp_atom_create(struct lisp_val v) {
  gc_push_root(v);
  struct lisp_atom *atom = lisp_obj_alloc(&ATOM_VTABLE, sizeof(*atom));
  atom->value = v;
  gc_pop_root_expect(v);
  return atom;
}

bool lisp_val_is_atom(struct lisp_val v) {
  return lisp_val_vtable(v) == &ATOM_VTABLE;
}

static void lisp_array_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  struct lisp_array *arr = lisp_val_as_obj(v);
  for (size_t i = 0; i < arr->length; i++) {
    cb(ctx, arr->data[i]);
  }
}

static const struct lisp_vtable ARRAY_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "array",
    .visit_children = lisp_array_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_array *lisp_array_create(size_t length) {
  struct lisp_array *arr = lisp_obj_alloc(
      &ARRAY_VTABLE, sizeof(*arr) + length * sizeof(arr->data[0]));
  arr->length = length;
  // TODO Write as memset(0) if the compiler isn't smart enough to figure it out
  for (size_t i = 0; i < length; i++) {
    arr->data[i] = LISP_VAL_NIL;
  }
  return arr;
}

bool lisp_val_is_array(struct lisp_val v) {
  return lisp_val_vtable(v) == &ARRAY_VTABLE;
}

static const struct lisp_vtable STRING_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "string",
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

static struct lisp_string *lisp_string_alloc(size_t capacity) {
  struct lisp_string *s = lisp_obj_alloc(&STRING_VTABLE, sizeof(*s) + capacity);
  s->length = 0;
  return s;
}

struct lisp_string *lisp_string_create(const char *s, size_t len) {
  struct lisp_string *str =
      lisp_obj_alloc(&STRING_VTABLE, sizeof(*str) + len + 1);
  str->length = len;
  memcpy(str->data, s, len);
  str->data[len] = 0;

  return str;
}

struct lisp_string *lisp_string_create_cstr(const char *s) {
  return lisp_string_create(s, strlen(s));
}

bool lisp_val_is_string(struct lisp_val v) {
  return lisp_val_vtable(v) == &STRING_VTABLE;
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
  str->length = n_chars;

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

bool lisp_string_eq(const struct lisp_string *a, const struct lisp_string *b) {
  if (a->length != b->length) {
    return false;
  }
  return memcmp(a->data, b->data, a->length) == 0;
}

static void str_builder_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  struct str_builder *builder = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(builder->buf));
}

static const struct lisp_vtable STR_BUILDER_VTABLE = {
    .name = "string-builder",
    .alloc_type = LISP_ALLOC_STACK,
    .visit_children = str_builder_visit,
    .destroy = lisp_destroy_none,
};

#define STR_BUILDER_INIT_CAP 8

void str_builder_init_cap_no_preserve(struct str_builder *b, size_t capacity) {
  b->header.vt = &STR_BUILDER_VTABLE;
  // Include null byte
  b->capacity = capacity + 1;
  b->buf = lisp_string_alloc(b->capacity);
}

void str_builder_init_no_preserve(struct str_builder *b) {
  str_builder_init_cap_no_preserve(b, STR_BUILDER_INIT_CAP);
}

void str_builder_init(struct str_builder *b) {
  str_builder_init_cap(b, STR_BUILDER_INIT_CAP);
}

void str_builder_init_cap(struct str_builder *b, size_t capacity) {
  str_builder_init_cap_no_preserve(b, capacity);
  gc_push_root_obj(b);
}

const struct lisp_string *str_builder_get_str(const struct str_builder *b) {
  return b->buf;
}

static void str_builder_ensure_cap(struct str_builder *b, size_t added_size) {
  size_t new_size = b->buf->length + added_size;
  if (new_size > b->capacity) {
    // TODO Avoid loop and just do max(cap*2, new_size)?
    do {
      b->capacity *= 2;
    } while (new_size > b->capacity);
    struct lisp_string *new_str = lisp_string_alloc(b->capacity);
    new_str->length = b->buf->length;
    memcpy(new_str->data, b->buf->data, b->buf->length);
    b->buf = new_str;
  }
}

void str_builder_append(struct str_builder *b, char c) {
  str_builder_ensure_cap(b, 1);
  b->buf->data[b->buf->length++] = c;
}

void str_builder_concat(struct str_builder *b, const struct lisp_string *s) {
  // TODO Save the string in case the buffer is reallocated?
  // gc_push_root_obj((void *)s);
  str_builder_concat_n(b, s->data, s->length);
  // gc_pop_root_expect_obj((void *)s);
}

void str_builder_concat_cstr(struct str_builder *b, const char *cstr) {
  while (*cstr != 0) {
    str_builder_append(b, *cstr);
    cstr++;
  }
}

void str_builder_concat_n(struct str_builder *b, const char *buf, size_t n) {
  str_builder_ensure_cap(b, n);
  memcpy(&b->buf->data[b->buf->length], buf, n);
  b->buf->length += n;
}

int str_builder_format(struct str_builder *b, const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  int result = str_builder_vformat(b, format, ap);
  va_end(ap);
  return result;
}

int str_builder_vformat(struct str_builder *b, const char *format, va_list ap) {
  // Try to build with existing capacity first
  char *format_buf = &b->buf->data[b->buf->length];
  size_t remaining_cap = b->capacity - b->buf->length;

  // Call vsnprintf with NULL buffer first to get the required buffer
  // size, then allocate the b->bufing and run it again.
  va_list ap_check_size;
  va_copy(ap_check_size, ap);
  int n_chars = vsnprintf(format_buf, remaining_cap, format, ap_check_size);
  va_end(ap_check_size);

  if (n_chars < 0) {
    return n_chars;
  }

  // Resize and try again
  // In the equal case snprintf wanted to write an extra NULL byte. Even though
  // b->buf_builder handles NULL terminators itself, snprintf may not write the
  // full content if it can't write the NULL byte.
  if ((unsigned)n_chars >= remaining_cap) {
    size_t req_cap = n_chars + 1;
    str_builder_ensure_cap(b, req_cap);
    format_buf = &b->buf->data[b->buf->length];

    int n_chars_actual = vsnprintf(format_buf, req_cap, format, ap);
    assert(n_chars_actual == n_chars);
    (void)n_chars_actual;
  }

  b->buf->length += n_chars;
  return n_chars;
}

struct lisp_string *str_build_no_preserve(struct str_builder *b) {
  // Add in null byte, but don't count in string size
  str_builder_ensure_cap(b, 1);
  b->buf->data[b->buf->length] = 0;
  // TODO Reallocate the string to truncate unused space

  return b->buf;
}

struct lisp_string *str_build(struct str_builder *b) {
  struct lisp_string *str = str_build_no_preserve(b);
  gc_pop_root_expect_obj(b);
  return str;
}

static void lisp_cons_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  const struct lisp_cons *cons = lisp_val_as_obj(v);
  cb(ctx, cons->car);
  cb(ctx, cons->cdr);
}

static const struct lisp_vtable CONS_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "cons",
    .visit_children = lisp_cons_visit,
    .destroy = lisp_destroy_none,
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

bool lisp_val_is_cons(struct lisp_val v) {
  return lisp_val_vtable(v) == &CONS_VTABLE;
}

bool lisp_val_is_list(struct lisp_val v) {
  while (lisp_val_is_cons(v)) {
    v = LISP_VAL_AS(struct lisp_cons, v)->cdr;
  }
  return lisp_val_is_nil(v);
}

unsigned lisp_list_count(struct lisp_val list) {
  unsigned count = 0;
  while (lisp_val_is_cons(list)) {
    list = LISP_VAL_AS(struct lisp_cons, list)->cdr;
    count++;
  }
  assert(lisp_val_is_nil(list));
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
  m->cursor = lisp_val_is_cons(original) ? lisp_val_as_obj(original) : NULL;
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
  while (lisp_val_is_cons(orig)) {
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
  } else if (lisp_val_is_cons(m->cursor->cdr)) {
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

static void lisp_closure_visit(struct lisp_val v, visit_callback cb,
                               void *ctx) {
  const struct lisp_closure *cl = lisp_val_as_obj(v);
  cb(ctx, lisp_val_from_obj(cl->code));
  for (size_t i = 0; i < cl->n_captures; i++) {
    cb(ctx, cl->captures[i]);
  }
}

static const struct lisp_vtable CLOSURE_VTABLE = {
    .alloc_type = LISP_ALLOC_GC,
    .name = "closure",
    .visit_children = lisp_closure_visit,
    .destroy = lisp_destroy_none,
};

struct lisp_closure *lisp_closure_create(struct code_chunk *bytecode,
                                         size_t n_captures) {
  gc_push_root_obj(bytecode);

  struct lisp_closure *cl = lisp_obj_alloc(
      &CLOSURE_VTABLE, sizeof(*cl) + sizeof(cl->captures[0]) * n_captures);
  cl->code = bytecode;
  cl->n_captures = n_captures;
  for (size_t i = 0; i < n_captures; i++) {
    // TODO Use some "undefined" value to better detect errors?
    cl->captures[i] = LISP_VAL_NIL;
  }

  gc_pop_root_expect_obj(bytecode);
  return cl;
}

bool lisp_val_is_func(struct lisp_val v) {
  return lisp_val_vtable(v) == &CLOSURE_VTABLE;
}

const char *lisp_closure_name_cstr(const struct lisp_closure *c) {
  return chunk_get_name(c->code);
}
