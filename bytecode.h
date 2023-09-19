#ifndef BYTECODE_H_
#define BYTECODE_H_

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "types.h"
#include "val_array.h"

enum bytecode_op {
  /**
   * Load a constant from the constant table.
   *
   * TODO Support more than 256 constants
   *
   * `const INDEX`
   */
  OP_CONST,

  /**
   * Resolve the top of the stack as a symbol and resolve that symbol in the
   * current environemnt. The resolved value replaces the symbol on top of the
   * stack.
   *
   * `lookup`
   */
  OP_LOOKUP,

  /**
   * Pops a symbol and a value off the stack and binds the symbol to the value
   * in the current environemnt.
   */
  OP_BIND,

  /**
   * Pops a value off the stack.
   */
  OP_POP,

  /**
   * Duplicates the top of the stack.
   */
  OP_DUP,

  /**
   * Call the top of the stack with the specified number of arguments.
   *
   * `call N-ARGS`
   */
  OP_CALL,

  /**
   * Clear the stack except for the top value and pop the current call frame.
   *
   * `return`
   */
  OP_RETURN,

  /**
   * Call the top of the stack with the specified number of arguments and
   * replace the current stack frame with the new one. Any non-argument values
   * in the stack will be deleted.
   *
   * `tail-call N-ARGS`
   */
  OP_TAIL_CALL,

  /**
   * Pop a code chunk off the top of the stack and push a newly created closure.
   *
   * `make-closure N-ARGS IS-VARIADIC`
   */
  OP_MAKE_CLOSURE,

  /**
   * Branch to another location in the current bytecode chunk.
   *
   * The offset is computed as a positive 2-byte integer relative to the index
   * after the current instruction.
   *
   * TODO Allow backwards jumps
   *
   * `branch OFFSET`
   */
  OP_BRANCH,

  /**
   * Pop the top of the stack and branch if it is falsey (nil).
   *
   * `branch-if-false OFFSET`
   */
  OP_BRANCH_IF_FALSE,
};

struct bytecode_array {
  size_t size;
  size_t cap;
  uint8_t *data;
};

// TODO Pick better name
struct code_chunk {
  struct lisp_obj header;
  struct val_array const_table;
  // TODO Store bytecode inline with the template allocation?
  struct bytecode_array bytecode;
};

struct code_chunk *chunk_create(void);
bool is_chunk(struct lisp_val v);

unsigned chunk_add_const(struct code_chunk *chunk, struct lisp_val v);
unsigned chunk_append_byte(struct code_chunk *chunk, uint8_t byte);
void chunk_set_byte(struct code_chunk *chunk, unsigned index, uint8_t byte);

// TODO Implement in lisp?
void chunk_disassemble(const struct code_chunk *chunk);

#endif
