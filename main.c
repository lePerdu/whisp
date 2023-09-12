#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"
#include "eval.h"
#include "file.h"
#include "log.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "readline/history.h"
#include "readline/readline.h"
#include "types.h"

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

static enum eval_status eval_print_many(struct lisp_val exprs) {
  gc_push_root(exprs);

  enum eval_status res = EV_SUCCESS;
  while (!lisp_val_is_nil(exprs)) {
    struct lisp_cons *cell = lisp_val_cast(LISP_CONS, exprs);
    assert(cell != NULL);
    exprs = cell->cdr;

    struct lisp_val result;
    res = eval(cell->car, global_env, &result);
    if (res == EV_EXCEPTION) {
      struct lisp_string *printed_exc = print_str(current_exception, true);
      log("error: %s", lisp_string_as_cstr(printed_exc));
      current_exception = LISP_VAL_NIL;
      break;
    } else {
      gc_push_root(result);
      struct lisp_string *printed = print_str(result, true);
      gc_pop_root_expect(result);

      display_str(printed);
      putchar('\n');
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

static void setup_environment(int argc, char **argv) {
  init_global_env();

  // TODO Make these constants or part of the reader?

  struct lisp_val true_val = lisp_true();
  gc_push_root(true_val);
  lisp_env_set(global_env, lisp_symbol_create_cstr("true"), true_val);
  gc_pop_root_expect(true_val);

  struct lisp_val false_val = lisp_false();
  gc_push_root(false_val);
  lisp_env_set(global_env, lisp_symbol_create_cstr("false"), false_val);
  gc_pop_root_expect(false_val);

  for (unsigned i = 0; builtins[i].func != NULL; i++) {
    struct lisp_symbol *sym = lisp_symbol_create_cstr(builtins[i].name);
    lisp_env_set(global_env, sym, lisp_val_from_obj(&builtins[i]));
  }

  struct lisp_val argv_list = create_argv_list(argc, argv);
  gc_push_root(argv_list);
  lisp_env_set(global_env, lisp_symbol_create_cstr("*ARGV*"), argv_list);
  gc_pop_root_expect(argv_list);
}

#define PRELUDE_FILENAME "lib/prelude.wh"

static void run_file(const char *filename) {
  enum eval_status res = load_file(filename);
  if (res != EV_SUCCESS) {
    struct lisp_string *printed_exc = print_str(current_exception, true);
    log("error: %s", lisp_string_as_cstr(printed_exc));
    exit(EXIT_FAILURE);
  }
}

static void load_prelude(void) { run_file(PRELUDE_FILENAME); }

static void rep(const char *input) {
  struct lisp_val ast;
  enum parse_res read_res = read_str_many(input, &ast);
  if (read_res == P_EMPTY) {
    return;
  } else if (read_res != P_SUCCESS) {
    fprintf(stderr, "Read error\n");
    return;
  }

  eval_print_many(ast);
}

static void repl(void) {
  init_repl();
  while (true) {
    char *input = read_prompt();

    if (input == NULL) {
      break;
    }

    rep(input);
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

  setup_environment(argc, argv);
  load_prelude();

  switch (mode) {
    case PROG_REPL:
      repl();
      break;
    case PROG_FILE: {
      run_file(filename);
      break;
    }
  }

  return 0;
}
