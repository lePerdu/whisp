#ifndef ENV_H_
#define ENV_H_

#include <stdbool.h>

#include "symbol.h"
#include "types.h"

/**
 * Key-value mapping used for the global environment.
 */
struct lisp_env;
struct lisp_env_binding;

struct lisp_env *lisp_env_create(void);
bool lisp_val_is_env(struct lisp_val v);

/**
 * Lookup a value in the environment. Returns NULL (not LISP_VAL_NIL) if the
 * symbol is not mapped.
 */
struct lisp_env_binding *lisp_env_get(struct lisp_env *env,
                                      struct lisp_symbol *sym);

struct lisp_env_binding *lisp_env_set(struct lisp_env *env,
                                      struct lisp_symbol *sym,
                                      struct lisp_val val);
struct lisp_env_binding *lisp_env_set_macro(struct lisp_env *env,
                                            struct lisp_symbol *sym,
                                            struct lisp_val val);

bool lisp_val_is_env_binding(struct lisp_val v);
const struct lisp_symbol *lisp_env_binding_name(
    const struct lisp_env_binding *b);
struct lisp_val lisp_env_binding_value(const struct lisp_env_binding *b);
void lisp_env_binding_set_value(struct lisp_env_binding *b,
                                struct lisp_val new);
bool lisp_env_binding_is_macro(const struct lisp_env_binding *b);

#endif
