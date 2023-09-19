#include "compiler_core.h"

#include <stdbool.h>
#include <stdint.h>

#include "bytecode.h"
#include "core_helper.h"
#include "types.h"
#include "vm.h"

DEF_BUILTIN(core_make_chunk) {
  BUILTIN_RETURN(lisp_val_from_obj(chunk_create()));
}

#define DEF_CHUNK_ARG(arg, index)                           \
  DEF_ARG(_raw_arg_##arg, (index));                         \
  if (!is_chunk(_raw_arg_##arg)) {                          \
    vm_raise_func_exception(vm, ERROR_ARG_TYPE, "chunk");   \
    return EV_EXCEPTION;                                    \
  }                                                         \
  struct code_chunk *arg = lisp_val_as_obj(_raw_arg_##arg); \
  (void)0

DEF_BUILTIN(core_chunk_size) {
  DEF_CHUNK_ARG(chunk, 0);
  BUILTIN_RETURN(lisp_val_from_int(chunk->bytecode.size));
}

DEF_BUILTIN(core_chunk_add_const) {
  DEF_CHUNK_ARG(chunk, 0);
  DEF_ARG(const_val, 1);

  unsigned const_index = chunk_add_const(chunk, const_val);
  BUILTIN_RETURN(lisp_val_from_int(const_index));
}

DEF_BUILTIN(core_chunk_append_byte) {
  DEF_CHUNK_ARG(chunk, 0);

  DEF_INT_ARG(byte, 1);
  if (byte < 0 || UINT8_MAX < byte) {
    vm_raise_func_exception(vm, "value out of bounds for byte");
    return EV_EXCEPTION;
  }

  unsigned instr_index = chunk_append_byte(chunk, byte);
  BUILTIN_RETURN(lisp_val_from_int(instr_index));
}

DEF_BUILTIN(core_chunk_set_byte) {
  DEF_CHUNK_ARG(chunk, 0);

  DEF_INT_ARG(index, 1);
  if (index < 0 || (long)chunk->bytecode.size <= index) {
    vm_raise_func_exception(vm, "index out of bounds");
    return EV_EXCEPTION;
  }

  DEF_INT_ARG(byte, 2);
  if (byte < 0 || UINT8_MAX < byte) {
    vm_raise_func_exception(vm, "value out of bounds for byte");
    return EV_EXCEPTION;
  }

  chunk_set_byte(chunk, index, byte);
  BUILTIN_RETURN(LISP_VAL_NIL);
}

DEF_BUILTIN(core_chunk_disassemble) {
  DEF_CHUNK_ARG(chunk, 0);

  chunk_disassemble(chunk);
  BUILTIN_RETURN(LISP_VAL_NIL);
}

static struct lisp_builtin compiler_builtins[] = {
    lisp_builtin_make("make-chunk", core_make_chunk, 0, false),
    lisp_builtin_make("chunk-size", core_chunk_size, 1, false),
    lisp_builtin_make("chunk-add-const!", core_chunk_add_const, 2, false),
    lisp_builtin_make("chunk-write-byte!", core_chunk_append_byte, 2, false),
    lisp_builtin_make("chunk-set-byte!", core_chunk_set_byte, 3, false),
    lisp_builtin_make("chunk-disassemble", core_chunk_disassemble, 1, false),
    lisp_builtin_make(NULL, NULL, 0, false),
};

void define_compiler_builtins(struct lisp_env *global_env) {
  for (unsigned i = 0; compiler_builtins[i].func != NULL; i++) {
    struct lisp_symbol *sym =
        lisp_symbol_create_cstr(compiler_builtins[i].name);
    lisp_env_set(global_env, sym, lisp_val_from_obj(&compiler_builtins[i]));
  }

#define DEFINE_CONST(lisp_name, enum_val)                      \
  lisp_env_set(global_env, lisp_symbol_create_cstr(lisp_name), \
               lisp_val_from_int(enum_val));

  DEFINE_CONST("*bytecode-const*", OP_CONST);
  DEFINE_CONST("*bytecode-lookup*", OP_LOOKUP);
  DEFINE_CONST("*bytecode-bind*", OP_BIND);
  DEFINE_CONST("*bytecode-pop*", OP_POP);
  DEFINE_CONST("*bytecode-dup*", OP_DUP);
  DEFINE_CONST("*bytecode-call*", OP_CALL);
  DEFINE_CONST("*bytecode-tail-call*", OP_TAIL_CALL);
  DEFINE_CONST("*bytecode-return*", OP_RETURN);
  DEFINE_CONST("*bytecode-make-closure*", OP_MAKE_CLOSURE);
  DEFINE_CONST("*bytecode-branch*", OP_BRANCH);
  DEFINE_CONST("*bytecode-branch-if-false*", OP_BRANCH_IF_FALSE);

#undef DEFINE_CONST
}
