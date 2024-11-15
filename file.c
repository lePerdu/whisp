#include "file.h"

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "compiler.h"
#include "eval.h"
#include "memory.h"
#include "printer.h"
#include "reader.h"
#include "types.h"
#include "vm.h"

struct lisp_string *read_file(struct lisp_vm *vm, const char *filename) {
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    vm_raise_func_exception(vm, "failed to open file '%s': %s", filename,
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
        vm_raise_func_exception(vm, "error reading file '%s': %s", filename,
                                strerror(errno));
        error = true;
        break;
      }  // Else, it's EOF so the loop will end
    } else {
      str_builder_concat_n(&builder, buf, n_read);
    }
  }

  if (fclose(file) != 0) {
    vm_raise_func_exception(vm, "error closing file '%s': %s", filename,
                            strerror(errno));
    error = true;
  }

  struct lisp_string *result = str_build(&builder);
  return error ? NULL : result;
}

#define READ_LINE_BUF_INITIAL_CAP 16

struct lisp_string *read_line(FILE *stream) {
  struct str_builder builder;
  str_builder_init_cap(&builder, READ_LINE_BUF_INITIAL_CAP);

  while (true) {
    char *buf = str_builder_raw_buf_end(&builder);
    size_t remaining_cap = str_builder_remaining_cap(&builder);
    if (fgets(buf, remaining_cap, stream) == NULL) {
      if (ferror(stream)) {
        return NULL;
      } else {
        // For EOF, can check later if data was read or not
        break;
      }
    }

    size_t n_read = strlen(buf);
    str_builder_include_size(&builder, n_read);

    assert(n_read > 0);
    // Check last character to know if reading stopped because of a full buffer
    // or a newline
    // TODO Wider check for EOL?
    if (str_builder_raw_buf_end(&builder)[-1] == '\n') {
      break;
    } else {
      // Expand the buffer before continuing
      str_builder_ensure_cap(&builder, builder.capacity);
    }
  }

  struct lisp_string *line = str_build(&builder);
  if (line->length == 0) {
    return NULL;
  } else {
    return line;
  }
}

struct parse_output *parse_file(struct lisp_vm *vm, struct lisp_string *filename) {
  gc_push_root_obj(filename);
  struct lisp_string *contents = read_file(vm, lisp_string_as_cstr(filename));
  gc_pop_root_expect_obj(filename);

  if (contents == NULL) {
    // Exception is set by read_file
    return NULL;
  }

  gc_push_root_obj(contents);
  struct parse_output *parsed =
      read_str_many(filename, lisp_string_as_cstr(contents));
  gc_pop_root_expect_obj(contents);

  if (parsed->status == P_ERROR) {
    vm_raise_exception(vm, lisp_val_from_obj(parse_error_format(parsed)));
    return NULL;
  }

  return parsed;
}

struct lisp_closure *compile_file(struct lisp_vm *vm,
                                  struct lisp_string *filename) {
  gc_push_root_obj(filename);
  struct lisp_string *contents = read_file(vm, lisp_string_as_cstr(filename));
  gc_pop_root_expect_obj(filename);

  if (contents == NULL) {
    // Exception is set by read_file
    return NULL;
  }

  return compile_string(vm, filename, contents);
}

struct lisp_closure *compile_string(struct lisp_vm *vm,
                                    struct lisp_string *filename,
                                    struct lisp_string *contents) {
  gc_push_root_obj(contents);
  struct parse_output *parsed =
      read_str_many(filename, lisp_string_as_cstr(contents));
  gc_pop_root_expect_obj(contents);
  if (parsed->status == P_ERROR) {
    vm_raise_exception(vm, lisp_val_from_obj(parse_error_format(parsed)));
    return NULL;
  }

  gc_push_root_obj(parsed);
  struct lisp_val wrapped_with_do = lisp_val_from_obj(
      lisp_cons_create(lisp_val_from_obj(SYMBOL_DO), parsed->datum));
  gc_pop_root_expect_obj(parsed);

  struct lisp_closure *compiled =
      compile_top_level(vm, parsed, wrapped_with_do);
  if (compiled == NULL) {
    return NULL;
  }

  return compiled;
}

bool file_exists(const char *filename) {
  FILE *file = fopen(filename, "r");
  if (file != NULL) {
    fclose(file);
    return true;
  } else {
    return false;
  }
}

bool delete_file(const char *filename) {
  if (remove(filename) != 0) {
    // TODO Report error message
    return false;
  } else {
    return true;
  }
}

#define INITIAL_CURRENT_DIRECTORY_SIZE 64

struct lisp_string *get_current_directory(void) {
  // Use a string builder since the path length isn't known.
  // The buffer is progressively grown as needed
  struct str_builder builder;
  str_builder_init_cap(&builder, INITIAL_CURRENT_DIRECTORY_SIZE);
  const char *ret;
  while ((ret = getcwd(str_builder_raw_buf(&builder), builder.capacity)) ==
         NULL) {
    str_builder_ensure_cap(&builder, builder.capacity * 2);
  }

  str_builder_include_size(&builder, strlen(ret));
  return str_build(&builder);
}
