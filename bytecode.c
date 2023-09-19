#include "bytecode.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "printer.h"
#include "types.h"
#include "val_array.h"

#define INIT_CAP 8

static void bytecode_array_init(struct bytecode_array *arr) {
  arr->size = 0;
  arr->cap = 0;
  arr->data = NULL;
}

static void bytecode_array_destroy(struct bytecode_array *arr) {
  free(arr->data);
}

// static uint8_t bytecode_array_get(const struct bytecode_array *s,
//                                   unsigned index) {
//   assert(index < s->size);
//   return s->data[index];
// }

static void bytecode_array_push(struct bytecode_array *s, uint8_t b) {
  if (s->size >= s->cap) {
    s->cap = s->cap < INIT_CAP ? INIT_CAP : 2 * s->cap;
    s->data = realloc(s->data, s->cap * sizeof(s->data[0]));
  }

  s->data[s->size++] = b;
}

static void code_chunk_visit(struct lisp_val v, visit_callback cb, void *ctx) {
  struct code_chunk *chunk = lisp_val_as_obj(v);
  val_array_visit(&chunk->const_table, cb, ctx);
}

static void code_chunk_destroy(struct lisp_val v) {
  struct code_chunk *chunk = lisp_val_as_obj(v);
  val_array_destroy(&chunk->const_table);
  bytecode_array_destroy(&chunk->bytecode);
}

static const struct lisp_vtable CODE_CHUNK_VTABLE = {
    .type = LISP_OPAQUE,
    .name = "code-chunk",
    .is_gc_managed = true,
    .visit_children = code_chunk_visit,
    .destroy = code_chunk_destroy,
};

struct code_chunk *chunk_create(void) {
  struct code_chunk *chunk = lisp_obj_alloc(&CODE_CHUNK_VTABLE, sizeof(*chunk));
  chunk->const_table = VAL_ARRAY_EMPTY;
  bytecode_array_init(&chunk->bytecode);
  return chunk;
}

bool is_chunk(struct lisp_val v) {
  return lisp_val_vtable(v) == &CODE_CHUNK_VTABLE;
}

unsigned chunk_add_const(struct code_chunk *t, struct lisp_val v) {
  val_array_push(&t->const_table, v);
  return t->const_table.size - 1;
}

unsigned chunk_append_byte(struct code_chunk *t, uint8_t byte) {
  bytecode_array_push(&t->bytecode, byte);
  return t->bytecode.size - 1;
}

void chunk_set_byte(struct code_chunk *t, unsigned index, uint8_t byte) {
  assert(index < t->bytecode.size);
  t->bytecode.data[index] = byte;
}

static int disassemble_instr(const struct code_chunk *chunk, unsigned offset) {
  if (offset >= chunk->bytecode.size) {
    return 0;
  }

#define ENSURE_INSTR_ARGS(opcode, n)                               \
  do {                                                             \
    if (offset + (n) >= chunk->bytecode.size) {                    \
      printf("\nnot enough arguments for opcode: %u\n", (opcode)); \
      return -1;                                                   \
    }                                                              \
  } while (false)

  printf("\t%04u\t", offset);
  enum bytecode_op opcode = chunk->bytecode.data[offset];
  switch (opcode) {
    case OP_CONST: {
      ENSURE_INSTR_ARGS(OP_CONST, 1);
      unsigned const_index = chunk->bytecode.data[offset + 1];
      if (const_index >= chunk->const_table.size) {
        printf("\ninvalid constant index: %u\n", const_index);
        return -1;
      }

      struct lisp_val const_val = chunk->const_table.data[const_index];
      printf("(const %s)\t; const index = %u\n",
             lisp_string_as_cstr(print_str(const_val, true)), const_index);
      return 2;
    }
    case OP_LOOKUP:
      printf("(lookup)\n");
      return 1;
    case OP_BIND:
      printf("(bind)\n");
      return 1;
    case OP_POP:
      printf("(pop)\n");
      return 1;
    case OP_DUP:
      printf("(dup)\n");
      return 1;
    case OP_CALL: {
      ENSURE_INSTR_ARGS(OP_CALL, 1);
      unsigned arg_count = chunk->bytecode.data[offset + 1];
      printf("(call %u)\n", arg_count);
      return 2;
    }
    case OP_RETURN:
      printf("(return)\n");
      return 1;
    case OP_TAIL_CALL: {
      ENSURE_INSTR_ARGS(OP_TAIL_CALL, 1);
      unsigned arg_count = chunk->bytecode.data[offset + 1];
      printf("(tail-call %u)\n", arg_count);
      return 2;
    }
    case OP_MAKE_CLOSURE:
      ENSURE_INSTR_ARGS(OP_MAKE_CLOSURE, 2);
      uint8_t arg_count = chunk->bytecode.data[offset + 1];
      uint8_t variadic = chunk->bytecode.data[offset + 2];
      printf("(make-closure %u %s)\n", arg_count, variadic ? "true" : "false");
      return 3;
    case OP_BRANCH: {
      ENSURE_INSTR_ARGS(OP_BRANCH, 2);
      // Read as litle endian
      uint16_t branch_offset = chunk->bytecode.data[offset + 1] +
                               (chunk->bytecode.data[offset + 2] << 8);
      unsigned branch_target = offset + 1 + branch_offset;
      printf("(branch %u)\t; target = %u\n", branch_offset, branch_target);
      if (branch_target >= chunk->bytecode.size) {
        printf("\nbranch target out of bounds: %u\n", branch_target);
        return -1;
      }
      return 3;
    }
    case OP_BRANCH_IF_FALSE: {
      ENSURE_INSTR_ARGS(OP_BRANCH_IF_FALSE, 2);
      // Read as litle endian
      unsigned branch_offset = chunk->bytecode.data[offset + 1] +
                               (chunk->bytecode.data[offset + 2] << 8);
      unsigned branch_target = offset + 1 + branch_offset;
      printf("(branch-if-false %u)\t; target = %u\n", branch_offset,
             branch_target);
      if (branch_target >= chunk->bytecode.size) {
        printf("\nbranch target out of bounds: %u\n", branch_target);
        return -1;
      }
      return 3;
    }
  }

#undef ENSURE_INSTR_ARGS

  printf("\ninvalid opcode at offset %04u: %u\n", offset, opcode);
  return -1;
}

void chunk_disassemble(const struct code_chunk *chunk) {
  printf("disassembly of code chunk at %p:\n", chunk);
  unsigned offset = 0;
  while (true) {
    int result = disassemble_instr(chunk, offset);
    if (result > 0) {
      offset += result;
    } else {
      break;
    }
  }
}
