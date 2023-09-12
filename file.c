#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "eval.h"
#include "file.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"

struct lisp_string *read_file(const char *filename) {
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    set_func_exception("failed to open file '%s': %s", filename,
                       strerror(errno));
    return NULL;
  }

  bool error = false;

  struct str_builder builder;
  str_builder_init(&builder);
  while (!feof(file)) {
    char buf[1024];
    size_t n_read = fread(buf, 1, sizeof(buf), file);
    if (n_read == 0) {
      if (ferror(file)) {
        set_func_exception("error reading file '%s': %s", filename,
                           strerror(errno));
        error = true;
        break;
      } // Else, it's EOF so the loop will end
    } else {
      str_builder_concat_n(&builder, buf, n_read);
    }
  }

  if (fclose(file) != 0) {
    set_func_exception("error closing file '%s': %s", filename,
                       strerror(errno));
    error = true;
  }

  struct lisp_string *result = str_build(&builder);
  return error ? NULL : result;
}

static enum eval_status eval_many(struct lisp_val exprs) {
  gc_push_root(exprs);

  enum eval_status res = EV_SUCCESS;
  while (!lisp_val_is_nil(exprs)) {
    struct lisp_cons *cell = lisp_val_cast(LISP_CONS, exprs);
    assert(cell != NULL);
    exprs = cell->cdr;

    struct lisp_val result;
    res = top_level_eval(cell->car, global_env, &result);
    if (res == EV_EXCEPTION) {
      break;
    }
  }

  gc_pop_root();
  return res;
}

enum eval_status load_file(const char *filename) {
  struct lisp_string *contents = read_file(filename);
  if (contents == NULL) {
    set_func_exception("cannot read file '%s'", filename);
    return EV_EXCEPTION;
  } else {
    gc_push_root_obj(contents);
    struct lisp_val ast;
    enum parse_res read_res =
        read_str_many(lisp_string_as_cstr(contents), &ast);
    gc_pop_root_expect_obj(contents);

    if (read_res != P_SUCCESS) {
      set_func_exception("cannot parse file '%s'", filename);
      return EV_EXCEPTION;
    }

    // Eval but don't print
    return eval_many(ast);
  }
}
