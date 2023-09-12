#ifndef MEMORY_H_
#define MEMORY_H_

#include <stddef.h>

#include "types.h"

void *lisp_alloc(size_t size);

void gc_push_root(struct lisp_val root);
void gc_pop_root();
void gc_pop_root_expect(struct lisp_val root);

void gc_push_root_obj(void *root);
void gc_pop_root_expect_obj(void *root);

/**
 * Allocate a new re-sized memory region. Unlike normal `realloc`, this does NOT
 * free/invalidate the old memory.
 * TODO Should this be a thing since it's just malloc + memcpy?
 */
// void *lisp_realloc(void *ptr, size_t size);

void lisp_gc_run(void);

#endif
