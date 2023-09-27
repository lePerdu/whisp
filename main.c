#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// These imports need to be after stdio.h but clang-format sorts them first
// clang-format off
#include <readline/history.h>
#include <readline/readline.h>
// clang-format on

#include "compiler.h"
#include "compiler_core.h"
#include "core.h"
#include "eval.h"
#include "file.h"
#include "log.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "types.h"
#include "vm.h"

/*
#define READLINE_BUF_SIZE 1024

static const char *readline(void) {
  static char buf[READLINE_BUF_SIZE];

  // TODO Check if input is too long
  if (fgets(buf, READLINE_BUF_SIZE, stdin) == NULL) {
    return NULL;
  }

  // TODO Allocate to new memory?
  return buf;
}
*/

#define HISTORY_FILENAME ".whisp_history"

static void init_repl(void) {
  using_history();
  read_history(HISTORY_FILENAME);
}

static char *read_prompt(void) {
  char *input = readline("> ");
  if (input == NULL) {
    return NULL;
  }

  add_history(input);
  append_history(1, HISTORY_FILENAME);
  return input;
}

static enum eval_status eval_print_many(struct lisp_vm *vm,
                                        struct lisp_val exprs) {
  gc_push_root(exprs);

  enum eval_status res = EV_SUCCESS;
  while (!lisp_val_is_nil(exprs)) {
    struct lisp_cons *cell = lisp_val_cast(LISP_CONS, exprs);
    assert(cell != NULL);
    exprs = cell->cdr;

    res = compile_eval(vm, cell->car);
    if (res == EV_SUCCESS) {
      // Keep on the stack while printing so it is't GC'd
      struct lisp_string *printed = print_str(vm_stack_top(vm), true);
      (void)vm_stack_pop(vm);

      display_str(printed);
      putchar('\n');
    } else {
      assert(vm_has_exception(vm));

      struct lisp_string *printed_exc =
          print_str(vm_current_exception(vm), true);
      vm_clear_exception(vm);

      log("error: %s", lisp_string_as_cstr(printed_exc));
      break;
    }
  }

  gc_pop_root();
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

  struct lisp_vm *vm = vm_create();
  // Save the VM permenantly
  gc_push_root_obj(vm);

  struct lisp_env *global_env = vm_current_env(vm);

  define_builtins(global_env);
  // Just for testing purposes
  // define_compiler_builtins(global_env);

  struct lisp_val argv_list = create_argv_list(argc, argv);
  gc_push_root(argv_list);
  lisp_env_set(global_env, lisp_symbol_create_cstr("*ARGV*"), argv_list);
  gc_pop_root_expect(argv_list);

  return vm;
}

#define PRELUDE_FILENAME "lib/prelude.wh"

static void run_file(struct lisp_vm *vm, const char *filename) {
  enum eval_status res = load_file(vm, filename);
  if (res == EV_EXCEPTION) {
    struct lisp_string *printed_exc = print_str(vm_current_exception(vm), true);
    vm_clear_exception(vm);
    log("error: %s", lisp_string_as_cstr(printed_exc));
    exit(EXIT_FAILURE);
  }
}

static void load_prelude(struct lisp_vm *vm) { run_file(vm, PRELUDE_FILENAME); }

static void rep(struct lisp_vm *vm, const char *input) {
  struct lisp_val ast;
  enum parse_res read_res = read_str_many(input, &ast);
  if (read_res == P_EMPTY) {
    return;
  } else if (read_res != P_SUCCESS) {
    fprintf(stderr, "Read error\n");
    return;
  }

  eval_print_many(vm, ast);
}

static void repl(struct lisp_vm *vm) {
  init_repl();
  while (true) {
    char *input = read_prompt();

    if (input == NULL) {
      break;
    }

    rep(vm, input);
    free(input);
  }
}

enum prog_mode {
  PROG_REPL,
  PROG_FILE,
};

int main(int argc, char *argv[]) {
  enum prog_mode mode = PROG_REPL;
  const char *filename = NULL;

  // TODO Proper argument parsing
  int consumed_args = 1;
  if (argc >= 2) {
    mode = PROG_FILE;
    filename = argv[1];
    consumed_args++;
  }

  argc -= consumed_args;
  argv += consumed_args;

  struct lisp_vm *vm = setup_vm(argc, argv);
  load_prelude(vm);

  switch (mode) {
    case PROG_REPL:
      repl(vm);
      break;
    case PROG_FILE: {
      run_file(vm, filename);
      break;
    }
  }

  return 0;
}
