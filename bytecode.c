#include "bytecode.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "env.h"
#include "printer.h"
#include "symbol.h"
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
  if (chunk->name != NULL) {
    cb(ctx, lisp_val_from_obj(chunk->name));
  }
  if (chunk->filename != NULL) {
    cb(ctx, lisp_val_from_obj(chunk->filename));
  }
  val_array_visit(&chunk->const_table, cb, ctx);
}

static void code_chunk_destroy(struct lisp_val v) {
  struct code_chunk *chunk = lisp_val_as_obj(v);
  val_array_destroy(&chunk->const_table);
  bytecode_array_destroy(&chunk->bytecode);
}

static const struct lisp_vtable CODE_CHUNK_VTABLE = {
    .name = "code-chunk",
    .alloc_type = LISP_ALLOC_GC,
    .visit_children = code_chunk_visit,
    .destroy = code_chunk_destroy,
};

struct code_chunk *chunk_create(void) {
  struct code_chunk *chunk = lisp_obj_alloc(&CODE_CHUNK_VTABLE, sizeof(*chunk));
  // TODO Set the function metadata at construction time?
  chunk->name = NULL;
  chunk->filename = NULL;
  chunk->req_arg_count = 0;
  chunk->is_variadic = false;

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

unsigned chunk_append_short(struct code_chunk *t, int16_t v) {
  unsigned offset = chunk_append_byte(t, v & 0xff);
  chunk_append_byte(t, v >> 8);
  return offset;
}

void chunk_set_byte(struct code_chunk *t, unsigned index, uint8_t byte) {
  assert(index < t->bytecode.size);
  t->bytecode.data[index] = byte;
}

void chunk_set_short(struct code_chunk *t, unsigned index, int16_t v) {
  chunk_set_byte(t, index, v & 0xff);
  chunk_set_byte(t, index + 1, v >> 8);
}

#define ENSURE_INSTR_ARGS(opcode, n)                               \
  do {                                                             \
    if (offset + (n) >= chunk->bytecode.size) {                    \
      printf("\nnot enough arguments for opcode: %u\n", (opcode)); \
      return -1;                                                   \
    }                                                              \
  } while (false)

static int disassemble_branch(const struct code_chunk *chunk,
                              enum bytecode_op op, const char *name,
                              unsigned offset) {
  ENSURE_INSTR_ARGS(op, 2);
  // Read as litle endian
  int16_t branch_offset = chunk->bytecode.data[offset + 1] +
                          (chunk->bytecode.data[offset + 2] << 8);
  int branch_target = offset + 1 + branch_offset;
  printf("(%s %u)\t; target = %u\n", name, branch_offset, branch_target);
  if (branch_target < 0 || (int)chunk->bytecode.size <= branch_target) {
    printf("\nbranch target out of bounds: %u\n", branch_target);
    return -1;
  }
  return 3;
}

static int disassemble_instr(const struct code_chunk *chunk, unsigned offset) {
  if (offset >= chunk->bytecode.size) {
    return 0;
  }

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
    case OP_GET_FP: {
      ENSURE_INSTR_ARGS(OP_GET_FP, 1);
      unsigned n = chunk->bytecode.data[offset + 1];
      printf("(get-fp %u)\n", n);
      return 2;
    }
    case OP_GET_UPVALUE: {
      ENSURE_INSTR_ARGS(OP_GET_UPVALUE, 1);
      unsigned n = chunk->bytecode.data[offset + 1];
      printf("(get-upvalue %u)\n", n);
      return 2;
    }
    case OP_GET_GLOBAL: {
      ENSURE_INSTR_ARGS(OP_GET_GLOBAL, 1);
      uint8_t const_index = chunk->bytecode.data[offset + 1];
      if (const_index >= chunk->const_table.size) {
        printf("\ninvalid constant index: %u\n", const_index);
        return -1;
      }
      if (!lisp_val_is_env_binding(chunk->const_table.data[const_index])) {
        printf("\nget-global argument must be an environment binding");
        return -1;
      }
      struct lisp_env_binding *binding =
          lisp_val_as_obj(chunk->const_table.data[const_index]);
      printf("(get-global %u)\t; sym = %s\n", const_index,
             lisp_symbol_name(lisp_env_binding_name(binding)));
      return 2;
    }
    case OP_SET_GLOBAL: {
      ENSURE_INSTR_ARGS(OP_SET_GLOBAL, 1);
      uint8_t const_index = chunk->bytecode.data[offset + 1];
      if (const_index >= chunk->const_table.size) {
        printf("\ninvalid constant index: %u\n", const_index);
        return -1;
      }
      if (!lisp_val_is_env_binding(chunk->const_table.data[const_index])) {
        printf("\nset-global argument must be an environment binding");
        return -1;
      }
      struct lisp_env_binding *binding =
          lisp_val_as_obj(chunk->const_table.data[const_index]);
      printf("(set-global %u)\t; sym = %s\n", const_index,
             lisp_symbol_name(lisp_env_binding_name(binding)));
      return 2;
    }
      printf("(set-global)\n");
      return 1;
    case OP_POP:
      printf("(pop)\n");
      return 1;
    case OP_SKIP_DELETE: {
      ENSURE_INSTR_ARGS(OP_SKIP_DELETE, 2);
      unsigned skip_n = chunk->bytecode.data[offset + 1];
      unsigned del_n = chunk->bytecode.data[offset + 2];
      printf("(skip-delete %u %u)\n", skip_n, del_n);
      return 3;
    }
    case OP_CALL: {
      ENSURE_INSTR_ARGS(OP_CALL, 1);
      unsigned arg_count = chunk->bytecode.data[offset + 1];
      printf("(call %u)\n", arg_count);
      return 2;
    }
    case OP_RETURN:
      printf("(return)\n");
      return 1;
    case OP_TAIL_CALL:
      printf("(tail-call)\n");
      return 1;
    case OP_ALLOC_CLOSURE: {
      ENSURE_INSTR_ARGS(OP_ALLOC_CLOSURE, 2);
      uint8_t const_index = chunk->bytecode.data[offset + 1];
      uint8_t n_captures = chunk->bytecode.data[offset + 2];
      if (const_index >= chunk->const_table.size) {
        printf("\ninvalid constant index: %u\n", const_index);
        return -1;
      }

      struct lisp_val const_val = chunk->const_table.data[const_index];
      printf("(alloc-closure %u %u)\t; const = %s\n", const_index, n_captures,
             lisp_string_as_cstr(print_str(const_val, true)));
      return 3;
    }
    case OP_INIT_CLOSURE: {
      ENSURE_INSTR_ARGS(OP_INTRINSIC, 1);
      uint8_t n_captures = chunk->bytecode.data[offset + 1];
      printf("(init-closure %u)\n", n_captures);
      return 2;
    }
    case OP_BUILD_REST_ARGS: {
      ENSURE_INSTR_ARGS(OP_BUILD_REST_ARGS, 1);
      uint8_t fp = chunk->bytecode.data[offset + 1];
      // TODO include intrinsic name
      printf("(build-rest-args %u)\n", fp);
      return 2;
    }
    case OP_INTRINSIC: {
      ENSURE_INSTR_ARGS(OP_INTRINSIC, 1);
      uint8_t index = chunk->bytecode.data[offset + 1];
      // TODO Fetch intrinsic name
      printf("(intrinsic %u)\n", index);
      return 2;
    }
    case OP_BRANCH:
      return disassemble_branch(chunk, OP_BRANCH, "branch", offset);
    case OP_BRANCH_IF_FALSE:
      return disassemble_branch(chunk, OP_BRANCH_IF_FALSE, "branch-if-false",
                                offset);
    case OP_GET_CURRENT_FRAME:
      printf("(get-current-frame)\n");
      return 1;
    case OP_RETURN_FROM_FRAME:
      printf("(return-from-frame)\n");
      return 1;
  }

  printf("\ninvalid opcode at offset %04u: %u\n", offset, opcode);
  return -1;
}

#undef ENSURE_INSTR_ARGS

const char *chunk_get_name(const struct code_chunk *chunk) {
  return chunk->name != NULL ? lisp_symbol_name(chunk->name) : "#<unnamed>";
}

void chunk_disassemble(const struct code_chunk *chunk) {
  printf("disassembly of code chunk '%s' at %p:\n", chunk_get_name(chunk),
         chunk);
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
