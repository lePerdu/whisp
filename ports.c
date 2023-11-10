#include "ports.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "eval.h"
#include "file.h"
#include "types.h"

inline bool lisp_val_is_file_open(struct lisp_file_port *port) {
  return port->file != NULL;
}

void lisp_file_port_close(struct lisp_file_port *port) {
  if (port->file != NULL) {
    // TODO Detect errors from fclose?
    fclose(port->file);
    port->file = NULL;
  }
}

static void lisp_file_port_destroy(struct lisp_val v) {
  struct lisp_file_port *port = lisp_val_as_obj(v);
  lisp_file_port_close(port);
}

static const struct lisp_vtable INPUT_FILE_PORT_VTABLE = {
    .name = "input-port",
    .alloc_type = LISP_ALLOC_GC,
    .visit_children = lisp_visit_none,
    .destroy = lisp_file_port_destroy,
};

static const struct lisp_vtable OUTPUT_FILE_PORT_VTABLE = {
    .name = "output-port",
    .alloc_type = LISP_ALLOC_GC,
    .visit_children = lisp_visit_none,
    .destroy = lisp_file_port_destroy,
};

struct lisp_file_port *lisp_input_file_port_create(FILE *stream) {
  struct lisp_file_port *port =
      lisp_obj_alloc(&INPUT_FILE_PORT_VTABLE, sizeof(*port));
  port->file = stream;
  return port;
}

struct lisp_file_port *lisp_input_file_port_open(const char *filename) {
  FILE *stream = fopen(filename, "r");
  if (stream == NULL) {
    // TODO Error message?
    return NULL;
  }

  return lisp_input_file_port_create(stream);
}

bool lisp_val_is_input_file_port(struct lisp_val v) {
  return lisp_val_vtable(v) == &INPUT_FILE_PORT_VTABLE;
}

enum eval_status lisp_input_file_port_read_char(struct lisp_file_port *port,
                                                struct lisp_val *output) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }

  int res = getc(port->file);
  if (res == EOF) {
    if (feof(port->file)) {
      *output = lisp_eof_object();
      return EV_SUCCESS;
    } else {
      return EV_EXCEPTION;
    }
  } else {
    *output = lisp_val_from_char(res);
    return EV_SUCCESS;
  }
}

enum eval_status lisp_input_file_port_peek_char(struct lisp_file_port *port,
                                                struct lisp_val *output) {
  if (lisp_input_file_port_read_char(port, output) == EV_EXCEPTION) {
    return EV_EXCEPTION;
  }
  if (lisp_val_is_eof_object(*output)) {
    return EV_SUCCESS;
  }

  int res = ungetc(lisp_val_as_char(*output), port->file);
  return res != EOF ? EV_SUCCESS : EV_EXCEPTION;
}

enum eval_status lisp_input_file_port_read_string(struct lisp_file_port *port,
                                                  size_t n,
                                                  struct lisp_val *output) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }

  // TODO Avoid allocating the whole string?
  struct lisp_string *buf = lisp_string_create_empty(n);
  size_t n_read = fread(buf->data, 1, n, port->file);
  if (n_read < n) {
    if (ferror(port->file)) {
      return EV_EXCEPTION;
    } else if (n_read == 0 && feof(port->file)) {
      *output = lisp_eof_object();
      return EV_SUCCESS;
    }
  }
  buf->length = n_read;
  buf->data[buf->length] = 0;

  // TODO Re-alloc the string to save unused space?
  *output = lisp_val_from_obj(buf);
  return EV_SUCCESS;
}

enum eval_status lisp_input_file_port_read_line(struct lisp_file_port *port,
                                                struct lisp_val *output) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }

  struct lisp_string *line = read_line(port->file);
  if (line == NULL) {
    if (feof(port->file)) {
      *output = lisp_eof_object();
      return EV_SUCCESS;
    } else {
      return EV_EXCEPTION;
    }
  }

  *output = lisp_val_from_obj(line);
  return EV_SUCCESS;
}

struct lisp_file_port *lisp_output_file_port_create(FILE *stream) {
  struct lisp_file_port *port =
      lisp_obj_alloc(&OUTPUT_FILE_PORT_VTABLE, sizeof(*port));
  port->file = stream;
  return port;
}

struct lisp_file_port *lisp_output_file_port_open(const char *filename) {
  FILE *stream = fopen(filename, "w");
  if (stream == NULL) {
    // TODO Error message?
    return NULL;
  }

  return lisp_output_file_port_create(stream);
}

bool lisp_val_is_output_file_port(struct lisp_val v) {
  return lisp_val_vtable(v) == &OUTPUT_FILE_PORT_VTABLE;
}

enum eval_status lisp_output_file_port_write_char(struct lisp_file_port *port,
                                                  lisp_char_t c) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }
  return putc(c, port->file) != EOF ? EV_SUCCESS : EV_EXCEPTION;
}

enum eval_status lisp_output_file_port_write_string(struct lisp_file_port *port,
                                                    struct lisp_string *s) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }

  size_t written = fwrite(s->data, 1, s->length, port->file);
  return written == s->length ? EV_SUCCESS : EV_EXCEPTION;
}

enum eval_status lisp_output_file_port_flush(struct lisp_file_port *port) {
  if (!lisp_val_is_file_open(port)) {
    return EV_EXCEPTION;
  }

  if (fflush(port->file) == 0) {
    return EV_SUCCESS;
  } else {
    return EV_EXCEPTION;
  }
}

static const struct lisp_vtable EOF_OBJECT_VTABLE = {
    .name = "eof-object",
    .alloc_type = LISP_ALLOC_CONST,
    .visit_children = lisp_visit_none,
    .destroy = lisp_destroy_none,
};

struct lisp_val lisp_eof_object(void) {
  static struct lisp_obj EOF_OBJECT = {
      .vt = &EOF_OBJECT_VTABLE,
  };
  return lisp_val_from_obj(&EOF_OBJECT);
}

bool lisp_val_is_eof_object(struct lisp_val v) {
  return lisp_val_vtable(v) == &EOF_OBJECT_VTABLE;
}
