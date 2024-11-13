#include <stdlib.h>

#include "config.h"

const char *get_whisp_lib_dir(void) {
  const char *env_var = getenv("WHISP_LIB_DIR");
  if (env_var != NULL) {
    return env_var;
  } else {
    return WHISP_DEFAULT_LIB_DIR;
  }
}
