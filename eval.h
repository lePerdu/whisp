#ifndef EVAL_H_
#define EVAL_H_

#include "types.h"

extern struct lisp_env *global_env;

void init_global_env(void);

extern struct lisp_val current_exception;

void set_format_exception(const char *format, ...);

#define set_func_exception(message, ...) \
  set_format_exception("%s: " message, __func__ __VA_OPT__(, ) __VA_ARGS__)

enum eval_status eval(struct lisp_val ast, struct lisp_env *env,
                      struct lisp_val *result);

enum eval_status eval_apply(struct lisp_val func, struct lisp_val args,
                            struct lisp_val *result);

#endif
