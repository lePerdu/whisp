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
   * Pops a symbol and a value off the stack and binds the symbol to the value
   * in the global environemnt.
   *
   * This is generated by the compiler when compiling top-level definitions so
   * that the compiled closure can define global objects.
   */
  OP_BIND_GLOBAL,

  /**
   * Pops a value off the stack.
   */
  OP_POP,

  /**
   * Duplicates the top of the stack.
   *
   * `dup`
   */
  OP_DUP,

  /**
   * Duplicates the n-th item from the frame pointer (i.e. bottom of the stack).
   *
   * `dup-fp N`
   */
  OP_DUP_FP,

  /**
   * Clears all values off the current stack frame.
   *
   * `clear`
   */
  OP_CLEAR,

  /**
   * Clears the stack except for the top N elements.
   *
   * `skip-clear N`
   *
   * TODO Better name?
   */
  OP_SKIP_CLEAR,

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
   * Call the top of the stack with all the values on the stack passed as
   * arguments.
   *
   * `tail-call`
   */
  OP_TAIL_CALL,

  /**
   * Pop a code chunk off the top of the stack and push a newly created closure.
   *
   * TODO Include the arg count descriptors and name in the code chunk (or maybe
   * put some other structure on the stack)?
   *
   * `make-closure N-ARGS IS-VARIADIC`
   */
  OP_MAKE_CLOSURE,

  /**
   * Collapse arguments starting at a given FP index into a list.
   *
   * `build-rest-args FP-INDEX`
   */
  OP_BUILD_REST_ARGS,

  /**
   * Call an intrisic by index.
   * These all operate on the stack frame of the current function, but will not
   * create or delete stack frames. Current API limits to 256 intrinsics, but
   * that's probably a good limit anyway.
   *
   * `intrinsic INDEX`
   */
  OP_INTRINSIC,

  /**
   * Branch to another location in the current bytecode chunk.
   *
   * The offset is computed as a signed 2-byte integer relative to the index
   * after the current instruction.
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

  /**
   * Set exception handler to a location in the bytecode.
   *
   * Offset is relative, just like branch instructions.
   */
  OP_SET_EX_HANDLER,
};

struct bytecode_array {
  size_t size;
  size_t cap;
  uint8_t *data;
};

// TODO Pick better name
// TODO Should this be GC'd? It probably always has a single clear owner (i.e. a
// function object)
struct code_chunk {
  struct lisp_obj header;
  struct val_array const_table;
  // TODO Store bytecode inline with the template allocation?
  struct bytecode_array bytecode;
};

struct code_chunk *chunk_create(void);
bool is_chunk(struct lisp_val v);

/**
 * Add a constant to the function's constant table and return its index.
 */
unsigned chunk_add_const(struct code_chunk *chunk, struct lisp_val v);

/**
 * Append a byte to the chunk and return its index.
 */
unsigned chunk_append_byte(struct code_chunk *chunk, uint8_t byte);

/**
 * Set a byte at a specific index.
 */
void chunk_set_byte(struct code_chunk *chunk, unsigned index, uint8_t byte);

unsigned chunk_append_short(struct code_chunk *t, int16_t v);

void chunk_set_short(struct code_chunk *t, unsigned index, int16_t v);

// TODO Implement in lisp?
void chunk_disassemble(const struct code_chunk *chunk);

#endif
