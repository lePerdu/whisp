#ifndef PRINTER_H_
#define PRINTER_H_

#include "types.h"

struct lisp_string *print_str(struct lisp_val v, bool readable);
void print_str_into(struct str_builder *b, struct lisp_val v, bool readable);

/**
 * Utility for printing strings to the console (printf stops at embedded nulls).
 */
void display_str(const struct lisp_string *s);

#endif
