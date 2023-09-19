#ifndef VAL_ARRAY_H_
#define VAL_ARRAY_H_

#include <stddef.h>

#include "types.h"

struct val_array {
  // TODO Try storing top instead of size for performance
  size_t size;
  size_t cap;
  struct lisp_val *data;
};

#define VAL_ARRAY_EMPTY ((struct val_array){.size = 0, .cap = 0, .data = NULL})

void val_array_destroy(struct val_array *s);

struct lisp_val val_array_get(const struct val_array *s, unsigned index);
void val_array_push(struct val_array *s, struct lisp_val v);
struct lisp_val val_array_top(struct val_array *s);
struct lisp_val val_array_pop(struct val_array *s);
void val_array_skip_delete(struct val_array *s, unsigned skip_n,
                           unsigned delete_n);

void val_array_visit(struct val_array *arr, visit_callback cb, void *ctx);

#endif
