#include "core.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <time.h>

#include "bytecode.h"
#include "compiler.h"
#include "core_helper.h"
#include "eval.h"
#include "file.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "types.h"
#include "vm.h"

MAKE_INT_BINARY(add, +);
MAKE_INT_BINARY(sub, -);
MAKE_INT_BINARY(mul, *);

DEF_BUILTIN(core_int_div) {
  POP_INT_ARG(divisor);
  POP_INT_ARG(dividend);
  if (divisor == 0) {
    vm_raise_func_exception(vm, "cannot divide by 0");
    return EV_EXCEPTION;
  }
  BUILTIN_RETURN(lisp_val_from_int(dividend / divisor));
}

MAKE_INT_BINARY(bitand, &);
MAKE_INT_BINARY(bitor, |);
MAKE_INT_BINARY(bitxor, ^);

static enum eval_status core_int_bitshift(struct lisp_vm *vm) {
  POP_INT_ARG(shift);
  POP_INT_ARG(x);

  BUILTIN_RETURN(lisp_val_from_int(
      shift >= 0 ? (unsigned long)(x << shift)
                 // Cast to unsigned to avoid sign extension
                 // TODO Figure out if this is necessary
                 : ((unsigned long)x >> (unsigned long)-shift)));
}

MAKE_INT_COMPARE(lt, <);
MAKE_INT_COMPARE(lte, <=);
MAKE_INT_COMPARE(gt, >);
MAKE_INT_COMPARE(gte, >=);
MAKE_INT_COMPARE(eq, ==);

MAKE_REAL_BINARY(add, +);
MAKE_REAL_BINARY(sub, -);
MAKE_REAL_BINARY(mul, *);
// No special handling for floating-point divide
MAKE_REAL_BINARY(div, /);

DEF_BUILTIN(core_real_exp) {
  POP_REAL_ARG(x);
  BUILTIN_RETURN(lisp_val_from_real(exp(x)));
}

DEF_BUILTIN(core_real_log) {
  POP_REAL_ARG(x);
  BUILTIN_RETURN(lisp_val_from_real(log(x)));
};

DEF_BUILTIN(core_real_pow) {
  POP_REAL_ARG(x);
  POP_REAL_ARG(b);
  BUILTIN_RETURN(lisp_val_from_real(pow(b, x)));
}

MAKE_REAL_COMPARE(lt, <);
MAKE_REAL_COMPARE(lte, <=);
MAKE_REAL_COMPARE(gt, >);
MAKE_REAL_COMPARE(gte, >=);
MAKE_REAL_COMPARE(eq, ==);

DEF_BUILTIN(core_identical) {
  POP_ARG(b);
  POP_ARG(a);
  BUILTIN_RETURN(lisp_val_from_bool(lisp_val_identical(a, b)));
}

DEF_BUILTIN(core_make_cons) {
  POP_ARG(cdr);
  POP_ARG(car);
  BUILTIN_RETURN(lisp_val_from_obj(lisp_cons_create(car, cdr)));
}

DEF_BUILTIN(core_car) {
  POP_OBJ_ARG(struct lisp_cons, arg, lisp_val_is_cons);
  BUILTIN_RETURN(arg->car);
}

DEF_BUILTIN(core_cdr) {
  POP_OBJ_ARG(struct lisp_cons, arg, lisp_val_is_cons);
  BUILTIN_RETURN(arg->cdr);
}

/**
 * Unlike other intrinsics, this takes the entire stack as its argument list.
 * That makes it basically only useful wrapped in a builtin function.
 */
DEF_BUILTIN(core_string_concat) {
  struct str_builder builder;
  str_builder_init(&builder);

  unsigned arg_count = vm_stack_size(vm);
  for (unsigned i = 0; i < arg_count; i++) {
    struct lisp_val next_arg = vm_from_frame_pointer(vm, i);
    struct lisp_string *s = lisp_val_cast(lisp_val_is_string, next_arg);
    if (s == NULL) {
      vm_raise_func_exception(vm, ERROR_STRING_ARG);
      goto ERROR;
    }
    str_builder_concat(&builder, s);
  }

  BUILTIN_RETURN(lisp_val_from_obj(str_build(&builder)));

ERROR:
  str_build(&builder);
  return EV_EXCEPTION;
}

enum eval_status string_count(struct lisp_val arg, struct lisp_val *result) {
  *result = lisp_val_from_int(LISP_VAL_AS(struct lisp_string, arg)->length);
  return EV_SUCCESS;
}

DEF_BUILTIN(core_string_count) {
  POP_OBJ_ARG(struct lisp_string, str, lisp_val_is_string);
  BUILTIN_RETURN(lisp_val_from_int(str->length));
}

DEF_BUILTIN(core_string_get) {
  POP_INT_ARG(index);
  POP_OBJ_ARG(struct lisp_string, str, lisp_val_is_string);

  if (index < 0 || (long)str->length <= index) {
    vm_raise_func_exception(vm, "string index out of bounds: %ld", index);
    return EV_EXCEPTION;
  }

  BUILTIN_RETURN(lisp_val_from_char(lisp_string_get(str, index)));
}

DEF_BUILTIN_PRED(core_is_cons, lisp_val_is_cons);
DEF_BUILTIN_PRED(core_is_null, lisp_val_is_nil);
DEF_BUILTIN_PRED(core_is_integer, lisp_val_is_int);
DEF_BUILTIN_PRED(core_is_real, lisp_val_is_real);

DEF_BUILTIN(core_to_real) {
  POP_ARG(arg);

  if (lisp_val_is_int(arg)) {
    BUILTIN_RETURN(lisp_val_from_real((double)lisp_val_as_int(arg)));
  } else if (lisp_val_is_real(arg)) {
    BUILTIN_RETURN(arg);
  } else {
    vm_raise_func_exception(vm, ERROR_NUMBER_ARG);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_to_int) {
  POP_ARG(arg);

  if (lisp_val_is_int(arg)) {
    BUILTIN_RETURN(arg);
  } else if (lisp_val_is_real(arg)) {
    BUILTIN_RETURN(lisp_val_from_int((long)lisp_val_as_real(arg)));
  } else {
    vm_raise_func_exception(vm, ERROR_NUMBER_ARG);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_string_to_symbol) {
  REF_OBJ_ARG(struct lisp_string, str, lisp_val_is_string, 0);

  const char *name = lisp_string_as_cstr(str);
  if (is_valid_symbol(name)) {
    struct lisp_symbol *sym = lisp_symbol_create(name, str->length);
    CLEAR_ARGS(1);
    BUILTIN_RETURN(lisp_val_from_obj(sym));
  } else {
    vm_raise_func_exception(vm, "invalid symbol name: '%s'", name);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN_PRED(core_is_symbol, lisp_val_is_symbol);
DEF_BUILTIN_PRED(core_is_char, lisp_val_is_char);
DEF_BUILTIN_PRED(core_is_string, lisp_val_is_string);

MAKE_CHAR_COMPARE(lt, <);
MAKE_CHAR_COMPARE(lte, <=);
MAKE_CHAR_COMPARE(gt, >);
MAKE_CHAR_COMPARE(gte, >=);
MAKE_CHAR_COMPARE(eq, ==);

DEF_BUILTIN(core_char_to_int) {
  POP_CHAR_ARG(c);
  BUILTIN_RETURN(lisp_val_from_int(c));
}

DEF_BUILTIN(core_int_to_char) {
  POP_INT_ARG(int_val);

  // TODO Expand range when better char support is available
  if (LISP_CHAR_MIN <= int_val && int_val <= LISP_CHAR_MAX) {
    BUILTIN_RETURN(lisp_val_from_char((lisp_char_t)int_val));
  } else {
    vm_raise_func_exception(vm, "integer value out of range: %ld", int_val);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_string_eq) {
  POP_OBJ_ARG(struct lisp_string, b, lisp_val_is_string);
  POP_OBJ_ARG(struct lisp_string, a, lisp_val_is_string);
  BUILTIN_RETURN(lisp_val_from_bool(lisp_string_eq(a, b)));
}

DEF_BUILTIN_PRED(core_is_function, lisp_val_is_func);

/**
 * Return the name of a function, or `NIL` if the function is anonymous.
 */
DEF_BUILTIN(core_function_name) {
  POP_OBJ_ARG(struct lisp_closure, func, lisp_val_is_func);
  BUILTIN_RETURN(lisp_val_from_obj(func->code->name));
}

DEF_BUILTIN_PRED(core_is_atom, lisp_val_is_atom);

DEF_BUILTIN(core_make_atom) {
  POP_ARG(value);
  BUILTIN_RETURN(lisp_val_from_obj(lisp_atom_create(value)));
}

DEF_BUILTIN(core_deref) {
  POP_OBJ_ARG(struct lisp_atom, atom, lisp_val_is_atom);
  BUILTIN_RETURN(lisp_atom_deref(atom));
}

DEF_BUILTIN(core_reset) {
  POP_ARG(new_value);
  POP_OBJ_ARG(struct lisp_atom, atom, lisp_val_is_atom);
  lisp_atom_reset(atom, new_value);
  BUILTIN_RETURN(new_value);
}

DEF_BUILTIN_PRED(core_is_array, lisp_val_is_array);

DEF_BUILTIN(core_make_array) {
  POP_INT_ARG(length);

  if (length >= 0) {
    BUILTIN_RETURN(lisp_val_from_obj(lisp_array_create(length)));
  } else {
    vm_raise_func_exception(vm, "length must be non-negative");
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_array_length) {
  POP_OBJ_ARG(struct lisp_array, arr, lisp_val_is_array);
  BUILTIN_RETURN(lisp_val_from_int(arr->length));
}

DEF_BUILTIN(core_array_get) {
  POP_INT_ARG(index);
  POP_OBJ_ARG(struct lisp_array, arr, lisp_val_is_array);

  if (0 <= index && index < (long)arr->length) {
    BUILTIN_RETURN(lisp_array_get(arr, index));
  } else {
    vm_raise_func_exception(vm, "index out of bounds: %ld", index);
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_array_set) {
  POP_ARG(value);
  POP_INT_ARG(index);
  POP_OBJ_ARG(struct lisp_array, arr, lisp_val_is_array);

  if (0 <= index && index < (long)arr->length) {
    lisp_array_set(arr, index, value);
    BUILTIN_RETURN(LISP_VAL_NIL);
  } else {
    vm_raise_func_exception(vm, "index out of bounds: %ld", index);
    return EV_EXCEPTION;
  }
}

/**
 * Convert value to a string.
 */
DEF_BUILTIN(core_to_string) {
  REF_ARG(arg, 0);
  struct lisp_string *str = print_str(arg, false);
  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_val_from_obj(str));
}

/**
 * Convert value to an AST string.
 */
DEF_BUILTIN(core_write_str) {
  REF_ARG(arg, 0);
  struct lisp_string *str = print_str(arg, true);
  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_val_from_obj(str));
}

/**
 * Print AST.
 */
DEF_BUILTIN(core_write) {
  REF_ARG(arg, 0);
  display_str(print_str(arg, true));
  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_non_printing());
}

/**
 * Print values with ' ' separator.
 */
DEF_BUILTIN(core_newline) {
  putchar('\n');
  BUILTIN_RETURN(lisp_non_printing());
}

/**
 * Print value in non-readable format
 */
DEF_BUILTIN(core_display) {
  REF_ARG(arg, 0);
  display_str(print_str(arg, false));
  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_non_printing());
}

DEF_BUILTIN(core_flush) {
  fflush(stdout);
  BUILTIN_RETURN(lisp_non_printing());
}

DEF_BUILTIN(core_read_str) {
  REF_OBJ_ARG(struct lisp_string, arg, lisp_val_is_string, 0);
  struct lisp_val result;
  struct parse_res res =
      read_str("#<unknown>", lisp_string_as_cstr(arg), &result);
  if (res.status == P_SUCCESS) {
    CLEAR_ARGS(1);
    BUILTIN_RETURN(result);
  } else {
    vm_raise_exception(vm, lisp_val_from_obj(parse_error_format(&res.error)));
    return EV_EXCEPTION;
  }
}

DEF_BUILTIN(core_slurp) {
  REF_OBJ_ARG(struct lisp_string, filename, lisp_val_is_string, 0);

  struct lisp_string *contents = read_file(vm, lisp_string_as_cstr(filename));
  if (contents == NULL) {
    // Exception is set by read_file
    return EV_EXCEPTION;
  }

  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_val_from_obj(contents));
}

DEF_BUILTIN(core_load_file) {
  REF_OBJ_ARG(struct lisp_string, filename, lisp_val_is_string, 0);

  enum eval_status res = load_file(vm, lisp_string_as_cstr(filename));
  if (res == EV_EXCEPTION) {
    return res;
  } else {
    CLEAR_ARGS(1);
    BUILTIN_RETURN(lisp_non_printing());
  }
}

/**
 * Build a list representing the stack trace of the current execution.
 *
 * The head of the list is the most recent (current) stack frame. Each element
 * of the list is the function object which is executing in that frame.
 *
 * TODO Flip order?
 * TODO Return array instead of a list?
 * TODO Include more info in each stack frame.
 */
DEF_BUILTIN(core_backtrace) {
  struct list_builder builder;
  list_builder_init(&builder);

  for (unsigned i = 0; i < vm->call_frames.size; i++) {
    list_builder_append(&builder,
                        lisp_val_from_obj(vm->call_frames.data[i].func));
  }

  BUILTIN_RETURN(list_build(&builder));
}

DEF_BUILTIN(core_runtime) {
  clock_t t = clock();
  // TODO Convert to consistent unit?
  BUILTIN_RETURN(lisp_val_from_int(t));
}

DEF_BUILTIN(core_time_ms) {
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  BUILTIN_RETURN(lisp_val_from_int(ts.tv_sec * 1000 + ts.tv_nsec / 1000));
}

DEF_BUILTIN(core_sleep) {
  POP_INT_ARG(millis);

  long seconds = millis / 1000;
  long nanos = (millis - seconds * 1000) * 1000;
  struct timespec t = {.tv_sec = seconds, .tv_nsec = nanos};
  if (thrd_sleep(&t, NULL) < 0) {
    vm_raise_func_exception(vm, "sleep interrupted");
    return EV_EXCEPTION;
  } else {
    BUILTIN_RETURN(lisp_non_printing());
  }
}

DEF_BUILTIN(core_macroexpand_1) {
  REF_ARG(ast, 0);
  struct lisp_cons *ast_cons = lisp_val_cast(lisp_val_is_cons, ast);
  if (ast_cons == NULL) {
    goto RETURN_AST;
  }

  struct lisp_symbol *head_sym =
      lisp_val_cast(lisp_val_is_symbol, ast_cons->car);
  if (head_sym == NULL) {
    goto RETURN_AST;
  }

  // TODO This only works at the top level
  // That's probably fine (and I don't know how to fix it), but something to
  // note
  const struct lisp_env_binding *binding =
      lisp_env_get(vm->global_env, head_sym);
  if (binding == NULL || !lisp_env_binding_is_macro(binding)) {
    goto RETURN_AST;
  }

  // eval_apply will manage the memory from here
  CLEAR_ARGS(1);
  return eval_apply(vm, lisp_env_binding_value(binding), ast_cons->cdr);

RETURN_AST:
  CLEAR_ARGS(1);
  BUILTIN_RETURN(ast);
}

DEF_BUILTIN(core_disassemble) {
  REF_OBJ_ARG(struct lisp_closure, func, lisp_val_is_func, 0);
  chunk_disassemble(func->code);
  CLEAR_ARGS(1);
  BUILTIN_RETURN(lisp_non_printing());
}

DEF_BUILTIN(core_compile_to_closure) {
  POP_ARG(ast);
  struct lisp_closure *cl = compile_top_level(vm, ast);
  if (cl == NULL) {
    return EV_EXCEPTION;
  }

  BUILTIN_RETURN(lisp_val_from_obj(cl));
}

DEF_BUILTIN(core_prepare_apply) {
  POP_ARG(args);
  POP_ARG(func);
  // TODO Do these need to be GC-preserved? Currently pushing to the stack
  // doesn't trigger GC but that might change in the future

  // TODO Clear the stack instead of asserting?
  assert(vm_stack_size(vm) == 0);

  if (!lisp_val_is_func(func)) {
    vm_raise_func_exception(vm, ERROR_FUNC_ARG);
    return EV_EXCEPTION;
  }

  struct lisp_cons *args_cons;
  while ((args_cons = lisp_val_cast(lisp_val_is_cons, args)) != NULL) {
    vm_stack_push(vm, args_cons->car);
    args = args_cons->cdr;
  }

  // Improper list
  if (!lisp_val_is_nil(args)) {
    vm_raise_func_exception(vm, ERROR_LIST_ARG);
    return EV_EXCEPTION;
  }

  vm_stack_push(vm, func);
  return EV_SUCCESS;
}

typedef enum eval_status (*intrinsic_fn)(struct lisp_vm *vm);

struct builtin_config {
  const char *name;
  intrinsic_fn c_func;
  unsigned req_arg_count;
  bool is_variadic;
};

static const struct builtin_config builtins[] = {
    [INTRINSIC_IS_INTEGER] = {"int?", core_is_integer, 1, false},
    [INTRINSIC_TO_INT] = {"int", core_to_int, 1, false},
    [INTRINSIC_INT_ADD] = {"int+", core_int_add, 2, false},
    [INTRINSIC_INT_SUB] = {"int-", core_int_sub, 2, false},
    [INTRINSIC_INT_MUL] = {"int*", core_int_mul, 2, false},
    [INTRINSIC_INT_DIV] = {"int/", core_int_div, 2, false},
    [INTRINSIC_INT_BITAND] = {"bitand", core_int_bitand, 2, false},
    [INTRINSIC_INT_BITOR] = {"bitor", core_int_bitor, 2, false},
    [INTRINSIC_INT_BITXOR] = {"bitxor", core_int_bitxor, 2, false},
    [INTRINSIC_INT_BITSHIFT] = {"bitshift", core_int_bitshift, 2, false},

    [INTRINSIC_INT_LT] = {"int<", core_int_lt, 2, false},
    [INTRINSIC_INT_LTE] = {"int<=", core_int_lte, 2, false},
    [INTRINSIC_INT_GT] = {"int>", core_int_gt, 2, false},
    [INTRINSIC_INT_GTE] = {"int>=", core_int_gte, 2, false},
    [INTRINSIC_INT_EQ] = {"int=", core_int_eq, 2, false},

    [INTRINSIC_IS_REAL] = {"real?", core_is_real, 1, false},
    [INTRINSIC_TO_REAL] = {"real", core_to_real, 1, false},
    [INTRINSIC_REAL_ADD] = {"real+", core_real_add, 2, false},
    [INTRINSIC_REAL_SUB] = {"real-", core_real_sub, 2, false},
    [INTRINSIC_REAL_MUL] = {"real*", core_real_mul, 2, false},
    [INTRINSIC_REAL_DIV] = {"real/", core_real_div, 2, false},
    [INTRINSIC_REAL_LT] = {"real<", core_real_lt, 2, false},
    [INTRINSIC_REAL_LTE] = {"real<=", core_real_lte, 2, false},
    [INTRINSIC_REAL_GT] = {"real>", core_real_gt, 2, false},
    [INTRINSIC_REAL_GTE] = {"real>=", core_real_gte, 2, false},
    [INTRINSIC_REAL_EQ] = {"real=", core_real_eq, 2, false},

    [INTRINSIC_REAL_EXP] = {"e**", core_real_exp, 2, false},
    [INTRINSIC_REAL_LOG] = {"log", core_real_log, 2, false},
    [INTRINSIC_REAL_POW] = {"real**", core_real_pow, 2, false},

    // TODO Get consistent set of equality checks. Current setup is:
    // - =: compares by reference. Works for integers and symbols due to how
    // they are implemented
    // - equal?: structural comparison, working for strings and lists
    // TODO Make equality checks variadic (this one and others)?
    [INTRINSIC_IDENTICAL] = {"=", core_identical, 2, false},
    [INTRINSIC_STRING_TO_SYMBOL] = {"string->symbol", core_string_to_symbol, 1,
                                    false},
    [INTRINSIC_IS_SYMBOL] = {"symbol?", core_is_symbol, 1, false},
    [INTRINSIC_IS_FUNCTION] = {"fn?", core_is_function, 1, false},
    [INTRINSIC_FUNCTION_NAME] = {"fn-name", core_function_name, 1, false},
    [INTRINSIC_IS_NULL] = {"null?", core_is_null, 1, false},
    [INTRINSIC_MAKE_CONS] = {"cons", core_make_cons, 2, false},
    [INTRINSIC_IS_CONS] = {"cons?", core_is_cons, 1, false},
    [INTRINSIC_CAR] = {"car", core_car, 1, false},
    [INTRINSIC_CDR] = {"cdr", core_cdr, 1, false},

    [INTRINSIC_IS_CHAR] = {"char?", core_is_char, 1, false},
    // TODO Impl char comparisions via char->int?
    [INTRINSIC_CHAR_LT] = {"char<", core_char_lt, 2, false},
    [INTRINSIC_CHAR_LTE] = {"char<=", core_char_lte, 2, false},
    [INTRINSIC_CHAR_GT] = {"char>", core_char_gt, 2, false},
    [INTRINSIC_CHAR_GTE] = {"char>=", core_char_gte, 2, false},
    [INTRINSIC_CHAR_EQ] = {"char=", core_char_eq, 2, false},
    [INTRINSIC_CHAR_TO_INT] = {"char->int", core_char_to_int, 1, false},
    [INTRINSIC_INT_TO_CHAR] = {"int->char", core_int_to_char, 1, false},

    [INTRINSIC_IS_STRING] = {"string?", core_is_string, 1, false},
    [INTRINSIC_STRING_EQ] = {"string=", core_string_eq, 2, false},
    [INTRINSIC_STRING_COUNT] = {"string-count", core_string_count, 1, false},
    [INTRINSIC_STRING_GET] = {"string-get", core_string_get, 2, false},
    [INTRINSIC_STRING_CONCAT] = {"string-concat", core_string_concat, 0, true},
    [INTRINSIC_TO_STRING] = {"->string", core_to_string, 1, false},

    [INTRINSIC_IS_ATOM] = {"atom?", core_is_atom, 1, false},
    [INTRINSIC_MAKE_ATOM] = {"atom", core_make_atom, 1, false},
    [INTRINSIC_DEREF] = {"deref", core_deref, 1, false},
    [INTRINSIC_RESET] = {"reset!", core_reset, 2, false},

    [INTRINSIC_IS_ARRAY] = {"array?", core_is_array, 1, false},
    [INTRINSIC_MAKE_ARRAY] = {"make-array", core_make_array, 1, false},
    [INTRINSIC_ARRAY_LENGTH] = {"array-length", core_array_length, 1, false},
    [INTRINSIC_ARRAY_GET] = {"array-get", core_array_get, 2, false},
    [INTRINSIC_ARRAY_SET] = {"array-set!", core_array_set, 3, false},

    [INTRINSIC_READ_STR] = {"read-str", core_read_str, 1, false},
    [INTRINSIC_WRITE] = {"write", core_write, 1, false},
    [INTRINSIC_WRITE_STR] = {"write-str", core_write_str, 1, false},
    [INTRINSIC_DISPLAY] = {"display", core_display, 1, false},
    [INTRINSIC_NEWLINE] = {"newline", core_newline, 0, false},
    [INTRINSIC_FLUSH] = {"flush", core_flush, 0, false},
    [INTRINSIC_SLURP] = {"slurp", core_slurp, 1, false},
    [INTRINSIC_LOAD_FILE] = {"load-file", core_load_file, 1, false},
    [INTRINSIC_BACKTRACE] = {"backtrace", core_backtrace, 0, false},
    [INTRINSIC_RUNTIME] = {"runtime", core_runtime, 0, false},
    [INTRINSIC_TIME_MS] = {"time-ms", core_time_ms, 0, false},
    [INTRINSIC_SLEEP] = {"sleep", core_sleep, 1, false},

    [INTRINSIC_MACROEXPAND_1] = {"macroexpand-1", core_macroexpand_1, 1, false},
    [INTRINSIC_DISASSEMBLE] = {"disassemble", core_disassemble, 1, false},

    // These are intrinsics, but not exposed directly as functions
    [INTRINSIC_PERPARE_APPLY] = {NULL, core_prepare_apply, 2, false},
    [INTRINSIC_COMPILE_TO_CLOSURE] = {NULL, core_compile_to_closure, 1, false},
};

#define BUILTIN_COUNT (sizeof(builtins) / sizeof(builtins[0]))

// Not a 100% test, but as good as possible without some macro magic
static_assert(BUILTIN_COUNT == INTRINSIC_INVALID, "missing builtins");

static struct lisp_closure *make_builtin(const char *name, uint8_t index,
                                         unsigned req_arg_count,
                                         bool is_variadic) {
  struct code_chunk *chunk = chunk_create();
  chunk_append_byte(chunk, OP_INTRINSIC);
  chunk_append_byte(chunk, index);
  chunk_append_byte(chunk, OP_RETURN);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr(name);
  gc_pop_root_expect_obj(chunk);

  chunk->req_arg_count = req_arg_count;
  chunk->is_variadic = is_variadic;

  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_apply(void) {
  struct code_chunk *chunk = chunk_create();
  chunk_append_byte(chunk, OP_INTRINSIC);
  chunk_append_byte(chunk, INTRINSIC_PERPARE_APPLY);
  chunk_append_byte(chunk, OP_TAIL_CALL);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("apply");
  gc_pop_root_expect_obj(chunk);

  chunk->req_arg_count = 2;

  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_eval(void) {
  // TODO Expose `(compile)` and implement `(eval)` in the prelude?
  struct code_chunk *chunk = chunk_create();
  chunk_append_byte(chunk, OP_INTRINSIC);
  chunk_append_byte(chunk, INTRINSIC_COMPILE_TO_CLOSURE);
  chunk_append_byte(chunk, OP_TAIL_CALL);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("eval");
  gc_pop_root_expect_obj(chunk);

  chunk->req_arg_count = 1;

  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_with_exception_handler(void) {
  struct code_chunk *chunk = chunk_create();
  chunk->req_arg_count = 2;  // handler, thunk
  chunk_append_byte(chunk, OP_GET_FP);
  chunk_append_byte(chunk, 0);  // handler
  chunk_append_byte(chunk, OP_PUSH_EX_HANDLER);
  chunk_append_byte(chunk, OP_GET_FP);
  chunk_append_byte(chunk, 1);  // thunk
  // Call + return instead of tail call to preserve the exception handler state
  chunk_append_byte(chunk, OP_CALL);
  chunk_append_byte(chunk, 0);  // no args
  chunk_append_byte(chunk, OP_RETURN);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("with-exception-handler");
  gc_pop_root_expect_obj(chunk);

  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_with_escape_continuation(void) {
  struct code_chunk *esc_chunk = chunk_create();
  esc_chunk->req_arg_count = 1;
  gc_push_root_obj(esc_chunk);

  chunk_append_byte(esc_chunk, OP_GET_UPVALUE);
  chunk_append_byte(esc_chunk, 0);
  chunk_append_byte(esc_chunk, OP_ESCAPE_FRAME);

  struct code_chunk *chunk = chunk_create();
  chunk_append_byte(chunk, OP_ALLOC_CLOSURE);
  chunk_append_byte(chunk,
                    chunk_add_const(chunk, lisp_val_from_obj(esc_chunk)));
  chunk_append_byte(chunk, 1);  // 1 capture: the frame ID
  chunk_append_byte(chunk, OP_GET_CURRENT_FRAME);
  chunk_append_byte(chunk, OP_INIT_CLOSURE);
  chunk_append_byte(chunk, 1);  // 1 capture

  chunk_append_byte(chunk, OP_GET_FP);
  chunk_append_byte(chunk, 0);  // thunk argument
  chunk_append_byte(chunk, OP_SKIP_DELETE);
  chunk_append_byte(chunk, 2);  // thunk, escape closure
  chunk_append_byte(chunk, 1);  // thunk argument
  chunk_append_byte(chunk, OP_TAIL_CALL);

  gc_pop_root_expect_obj(esc_chunk);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("call-with-escape-continuation");
  gc_pop_root_expect_obj(chunk);

  chunk->req_arg_count = 1;

  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_raise(void) {
  struct code_chunk *chunk = chunk_create();
  chunk->req_arg_count = 1;  // Exception
  unsigned start_pos = chunk_append_byte(chunk, OP_GET_FP);
  // Copy the exception so it can be passed to the next handler if necessary
  chunk_append_byte(chunk, 0);  // Exception
  chunk_append_byte(chunk, OP_CALL_EX_HANDLER);
  chunk_append_byte(chunk, OP_POP);  // Ignore return
  // Re-raise by simply restarting the function
  chunk_append_byte(chunk, OP_BRANCH);
  unsigned branch_pos = chunk->bytecode.size;
  int16_t branch_offset = (int16_t)start_pos - (int16_t)branch_pos;
  chunk_append_short(chunk, branch_offset);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("raise");
  gc_pop_root_expect_obj(chunk);
  return lisp_closure_create(chunk, 0);
}

static struct lisp_closure *make_builtin_raise_continuable(void) {
  struct code_chunk *chunk = chunk_create();
  chunk->req_arg_count = 1;  // Exception
  // Exception is already in proper position for the call
  chunk_append_byte(chunk, OP_CALL_EX_HANDLER);
  chunk_append_byte(chunk, OP_RETURN);

  gc_push_root_obj(chunk);
  chunk->name = lisp_symbol_create_cstr("raise-continuable");
  gc_pop_root_expect_obj(chunk);
  return lisp_closure_create(chunk, 0);
}

enum eval_status call_intrinsic(uint8_t index, struct lisp_vm *vm) {
  assert(index < BUILTIN_COUNT);
  return builtins[index].c_func(vm);
}

static void define_const(struct lisp_env *env, const char *name,
                         struct lisp_val v) {
  gc_push_root(v);
  lisp_env_set(env, lisp_symbol_create_cstr(name), v);
  gc_pop_root_expect(v);
}

static void define_cl(struct lisp_env *env, struct lisp_closure *cl) {
  struct lisp_symbol *name = cl->code->name;
  assert(name != NULL);
  lisp_env_set(env, name, lisp_val_from_obj(cl));
}

static struct lisp_closure *BUILTIN_RAISE = NULL;

void init_global_builtins(void) {
  BUILTIN_RAISE = make_builtin_raise();
  gc_push_root_obj(BUILTIN_RAISE);
}

struct lisp_closure *get_builtin_raise(void) {
  assert(BUILTIN_RAISE != NULL);
  return BUILTIN_RAISE;
}

void define_builtins(struct lisp_env *global_env) {
  for (unsigned i = INTRINSIC_FN_START; i < INTRINSIC_FN_END; i++) {
    const struct builtin_config *b = &builtins[i];
    define_cl(global_env,
              make_builtin(b->name, i, b->req_arg_count, b->is_variadic));
  }

  // Special builtins
  define_cl(global_env, make_builtin_apply());
  define_cl(global_env, make_builtin_eval());
  define_cl(global_env, BUILTIN_RAISE);
  define_cl(global_env, make_builtin_raise_continuable());
  define_cl(global_env, make_builtin_with_exception_handler());
  define_cl(global_env, make_builtin_with_escape_continuation());

  // TODO Make these constants or part of the reader?
  define_const(global_env, "true", lisp_true());
  define_const(global_env, "false", lisp_false());
}
