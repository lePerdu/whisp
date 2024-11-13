#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "config.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "file.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "symbol.h"
#include "types.h"
#include "vm.h"

static struct lisp_string *read_prompt(void) {
  printf("(no-init)> ");
  struct lisp_string *input = read_line(stdin);
  if (input == NULL) {
    return NULL;
  }

  return input;
}

static enum eval_status eval_print_many(struct lisp_vm *vm,
                                        struct parse_output *parsed) {
  gc_push_root_obj(parsed);
  struct lisp_val exprs = parsed->datum;

  enum eval_status res = EV_SUCCESS;
  while (!lisp_val_is_nil(exprs)) {
    struct lisp_cons *cell = lisp_val_cast(lisp_val_is_cons, exprs);
    assert(cell != NULL);
    exprs = cell->cdr;

    res = compile_eval(vm, parsed, cell->car);
    if (res == EV_SUCCESS) {
      // Keep on the stack while printing so it is't GC'd
      struct lisp_val res_val = vm_stack_top(vm);
      struct lisp_string *printed = print_str(res_val, true);
      display_str(printed);
      putchar('\n');
      (void)vm_stack_pop(vm);
    } else {
      struct lisp_string *printed_exc =
          print_str(vm_current_exception(vm), false);
      vm_clear_exception(vm);

      printf("error: ");
      display_str(printed_exc);
      putchar('\n');
      break;
    }
  }

  gc_pop_root_expect_obj(parsed);
  return res;
}

static struct lisp_val create_argv_list(int argc, char *argv[]) {
  struct list_builder builder;
  list_builder_init(&builder);
  for (int i = 0; i < argc; i++) {
    list_builder_append(&builder,
                        lisp_val_from_obj(lisp_string_create_cstr(argv[i])));
  }
  return list_build(&builder);
}

static struct lisp_vm *setup_vm(int argc, char **argv) {
  init_global_compile_state();

  struct lisp_vm *vm = vm_create(lisp_env_create());
  // Save the VM permanently
  gc_push_root_obj(vm);

  struct lisp_env *global_env = vm->global_env;

  define_builtins(global_env);

  struct lisp_val argv_list = create_argv_list(argc, argv);
  define_const(global_env, "*command-line*", argv_list);

  return vm;
}

#define STARTUP_FILENAME "boot.wh"

static enum eval_status load_file(struct lisp_vm *vm,
                                  struct lisp_string *filename) {
  gc_push_root_obj(filename);
  struct lisp_closure *compiled = compile_file(vm, filename);
  gc_pop_root_expect_obj(filename);
  if (compiled == NULL) {
    return EV_EXCEPTION;
  }

  return eval_closure(vm, compiled);
}

static void run_file(struct lisp_vm *vm, struct lisp_string *filename) {
  enum eval_status res = load_file(vm, filename);
  if (res == EV_EXCEPTION) {
    struct lisp_string *printed_exc =
        print_str(vm_current_exception(vm), false);
    vm_clear_exception(vm);
    printf("error: ");
    display_str(printed_exc);
    putchar('\n');
    exit(EXIT_FAILURE);
  }
}

static void load_startup_file(struct lisp_vm *vm) {
  struct str_builder b;
  str_builder_init(&b);
  str_builder_concat_cstr(&b, get_whisp_lib_dir());
  str_builder_append(&b, '/');
  str_builder_concat_cstr(&b, STARTUP_FILENAME);

  run_file(vm, str_build(&b));
}

static void rep(struct lisp_vm *vm, struct lisp_string *input) {
  gc_push_root_obj(input);
  struct parse_output *parsed = read_str_many(lisp_string_create_cstr("stdin"),
                                              lisp_string_as_cstr(input));
  gc_pop_root_expect_obj(input);

  switch (parsed->status) {
    case P_EMPTY:
      return;
    case P_SUCCESS:
      eval_print_many(vm, parsed);
      return;
    case P_ERROR: {
      struct lisp_string *err_str = parse_error_format(parsed);
      fprintf(stderr, "Read error: %s\n", lisp_string_as_cstr(err_str));
      return;
    }
  }
}

static void repl(struct lisp_vm *vm) {
  while (true) {
    struct lisp_string *input = read_prompt();
    if (input == NULL) {
      putchar('\n');
      break;
    }

    rep(vm, input);
  }
}

int main(int argc, char *argv[]) {
  struct lisp_vm *vm = setup_vm(argc, argv);

  // TODO Proper argument parsing?
  if (argc >= 2 && strcmp(argv[1], "-q") == 0) {
    // Simple REPL without loading the prelude
    repl(vm);
  } else {
    load_startup_file(vm);
  }

  return 0;
}
