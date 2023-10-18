#ifndef PORTS_H_
#define PORTS_H_

#include <stddef.h>
#include <stdio.h>

#include "eval.h"
#include "types.h"

struct lisp_file_port {
  struct lisp_obj header;
  FILE *file;
};

struct lisp_file_port *lisp_input_file_port_create(FILE *stream);
struct lisp_file_port *lisp_input_file_port_open(const char *filename);

bool lisp_val_is_input_file_port(struct lisp_val v);
enum eval_status lisp_input_file_port_read_char(struct lisp_file_port *port,
                                                struct lisp_val *output);
enum eval_status lisp_input_file_port_peek_char(struct lisp_file_port *port,
                                                struct lisp_val *output);
enum eval_status lisp_input_file_port_read_string(struct lisp_file_port *port,
                                                  size_t n,
                                                  struct lisp_val *output);
enum eval_status lisp_input_file_port_read_line(struct lisp_file_port *port,
                                                struct lisp_val *output);

struct lisp_file_port *lisp_output_file_port_create(FILE *stream);
struct lisp_file_port *lisp_output_file_port_open(const char *filename);

bool lisp_val_is_output_file_port(struct lisp_val v);
enum eval_status lisp_output_file_port_write_char(struct lisp_file_port *port,
                                                  lisp_char_t c);
enum eval_status lisp_output_file_port_write_string(struct lisp_file_port *port,
                                                    struct lisp_string *s);
enum eval_status lisp_output_file_port_flush(struct lisp_file_port *port);

bool lisp_val_is_file_port(struct lisp_val v);
bool lisp_val_is_file_open(struct lisp_file_port *port);
void lisp_file_port_close(struct lisp_file_port *port);

struct lisp_val lisp_eof_object(void);
bool lisp_val_is_eof_object(struct lisp_val v);

#endif
