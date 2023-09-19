#include "val_array.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define INIT_CAP 8

struct lisp_val val_array_get(const struct val_array *s, unsigned index) {
  assert(index < s->size);
  return s->data[index];
}

void val_array_push(struct val_array *s, struct lisp_val v) {
  if (s->size >= s->cap) {
    s->cap = s->cap < INIT_CAP ? INIT_CAP : 2 * s->cap;
    s->data = realloc(s->data, s->cap * sizeof(s->data[0]));
  }

  s->data[s->size++] = v;
}

struct lisp_val val_array_top(struct val_array *s) {
  assert(s->size > 0);
  return s->data[s->size - 1];
}

struct lisp_val val_array_pop(struct val_array *s) {
  assert(s->size > 0);
  return s->data[--s->size];
}

void val_array_skip_delete(struct val_array *s, unsigned skip_n,
                           unsigned delete_n) {
  assert(s->size >= delete_n + skip_n);
  memmove(&s->data[s->size - delete_n - skip_n], &s->data[s->size - skip_n],
          skip_n * sizeof(s->data[0]));
  s->size -= delete_n;
}

void val_array_destroy(struct val_array *s) { free(s->data); }

void val_array_visit(struct val_array *arr, visit_callback cb, void *ctx) {
  for (size_t i = 0; i < arr->size; i++) {
    cb(ctx, arr->data[i]);
  }
}
