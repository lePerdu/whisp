#ifndef LOG_H_
#define LOG_H_

#include <stdio.h>

#define log(message, ...)                                                      \
  fprintf(stderr, "%s: " message "\n", __func__ __VA_OPT__(, ) __VA_ARGS__)

#endif
