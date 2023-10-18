#include "memory.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "log.h"
#include "symbol.h"
#include "types.h"
#include "val_array.h"

// #define GC_DEBUG
// #define GC_LOG

/**
 * Metadata stored for each allocation.
 * This info is probably redundantly stored my malloc, but is not accessible in
 * a standard way (if any).
 */
struct alloc_header {
  size_t size;
  struct alloc_header *next;
  // TODO Store as bit in size or something
  bool marked;
};

#define ALLOC_DATA_OFFSET (sizeof(struct alloc_header))
static_assert((ALLOC_DATA_OFFSET & (alignof(struct lisp_obj) - 1)) == 0,
              "Allocation offset not properly aligned");

static void *header_to_obj(struct alloc_header *header) {
  return (void *)header + ALLOC_DATA_OFFSET;
}

static struct alloc_header *obj_to_header(void *obj) {
  return obj - ALLOC_DATA_OFFSET;
}

// Initialize and de-initialize as non-zero in debug mode to better detect and
// distinguish between invalid pointers and use-after-free.
#define GC_INIT_BYTE 0xAA
#define GC_UNINIT_BYTE 0x55

static struct alloc_header *first_alloc = NULL;
static size_t allocated_bytes = 0;

void *lisp_alloc(size_t size) {
  // Allow to run, but the GC might decide not to do anything
  lisp_gc_run();

  struct alloc_header *header = malloc(ALLOC_DATA_OFFSET + size);
  header->size = size;
  header->next = first_alloc;
  header->marked = false;
  first_alloc = header;
  allocated_bytes += size;

  void *obj = header_to_obj(header);

#ifdef GC_LOG
  log("%lu bytes at %p (header = %p)", size, obj, header);
#endif
#ifdef GC_DEBUG
  memset(obj, GC_INIT_BYTE, size);
#endif

  return obj;
}

static void alloc_header_destroy(struct alloc_header *header) {
  struct lisp_obj *obj = header_to_obj(header);
  obj->vt->destroy(lisp_val_from_obj(obj));

  allocated_bytes -= header->size;

#ifdef GC_LOG
  log("%lu bytes at %p (header = %p)", header->size, header_to_obj(header),
      header);
#endif
#ifdef GC_DEBUG
  memset(header, GC_UNINIT_BYTE, ALLOC_DATA_OFFSET + header->size);
#endif
  free(header);
}

// void *lisp_realloc(void *ptr, size_t size) { return realloc(ptr, size); }

static struct val_array gc_roots = VAL_ARRAY_EMPTY;

void gc_push_root(struct lisp_val root) { val_array_push(&gc_roots, root); }

void gc_pop_root() { val_array_pop(&gc_roots); }

void gc_pop_root_expect(struct lisp_val root) {
  struct lisp_val popped = val_array_pop(&gc_roots);
  (void)root;
  (void)popped;
  // Must be identical, just just equivalent
  assert(popped.tagged_ptr == root.tagged_ptr);
}

void gc_push_root_obj(void *root) { gc_push_root(lisp_val_from_obj(root)); }

void gc_pop_root_expect_obj(void *root) {
  gc_pop_root_expect(lisp_val_from_obj(root));
}

// TODO Make a local variable to clean up between invocations?
static struct val_array gray_set = VAL_ARRAY_EMPTY;

static void mark_gray(struct lisp_val v) {
  switch (lisp_val_vtable(v)->alloc_type) {
    case LISP_ALLOC_CONST:
      // These can't contain refrences to GC objects, so skip them
      return;
    case LISP_ALLOC_GC: {
      // Make sure it hasn't already been marked
      struct alloc_header *header = obj_to_header(lisp_val_as_obj(v));
      if (header->marked) {
        return;
      }

      header->marked = true;
      break;
    }
    case LISP_ALLOC_STACK:
      // No mark stored for these objects, so always push them
      // (They should only be referenced from the GC root stack, so this won't
      // cause infinite loops)
      break;
    default:
      abort();
  }

  val_array_push(&gray_set, v);
}

static void mark_gray_visit_cb(void *ctx, struct lisp_val v) {
  // Callback compatible with lisp_val_visit
  (void)ctx;
  mark_gray(v);
}

static void gc_clear_gray_set(void) {
  while (gray_set.size > 0) {
    struct lisp_val next = val_array_pop(&gray_set);

    lisp_val_vtable(next)->visit_children(next, mark_gray_visit_cb, NULL);
  }
}

static void gc_mark(void) {
  for (unsigned i = 0; i < gc_roots.size; i++) {
    mark_gray(gc_roots.data[i]);
  }

  gc_clear_gray_set();
}

static void gc_sweep(void) {
  struct alloc_header *prev = NULL;
  struct alloc_header *current = first_alloc;

  while (current != NULL) {
    struct alloc_header *next = current->next;
    if (current->marked) {
      current->marked = false;
      prev = current;
    } else {
      alloc_header_destroy(current);
      // Piece back together the linked list
      if (prev == NULL) {
        // Head was removed
        first_alloc = next;
      } else {
        prev->next = next;
      }
    }
    current = next;
  }
}

static bool symbol_not_marked(struct lisp_val sym) {
  struct alloc_header *header = obj_to_header(lisp_val_as_obj(sym));
  return !header->marked;
}

/**
 * Clean up and then mark the symbol intern table separately to implement its
 * "weak references".
 */
static void symbol_intern_table_gc(void) {
  // Remove un-marked entries
  lisp_symbol_intern_table_remove_if(symbol_not_marked);

  // Easiest way to ensure safety is to just re-run the mark routine.
  // Since it has already been run, it will only mark previously-unmarked
  // obejcts accessible from the symbol table (i.e. just the symbol table and
  // it's allocations)
  mark_gray(lisp_symbol_intern_table());
  gc_clear_gray_set();
}

// 500 KB
#define INIT_GC_THRESHOLD (1UL << 19)
#define GC_HEAP_GROW_FACTOR 4
static size_t gc_threshold_bytes = INIT_GC_THRESHOLD;

void lisp_gc_run(void) {
#ifndef GC_DEBUG
  if (allocated_bytes < gc_threshold_bytes) {
    return;
  }
#endif

#ifdef GC_LOG
  size_t init_allocated_bytes = allocated_bytes;
  log("begin; %lu bytes allocated", init_allocated_bytes);
#endif

  gc_mark();
  symbol_intern_table_gc();
  gc_sweep();

  gc_threshold_bytes = allocated_bytes * GC_HEAP_GROW_FACTOR;
#ifdef GC_LOG
  size_t freed_bytes = init_allocated_bytes - allocated_bytes;
  log("end; %lu bytes freed; %lu remaining", freed_bytes, allocated_bytes);
#endif
}
