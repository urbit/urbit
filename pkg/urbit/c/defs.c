/// @file defs.c

#include "c/defs.h"

extern inline ssize_t
c3_read(c3_i fd_i, void* const data_v, const size_t data_len_i);

extern inline ssize_t
c3_write(c3_i fd_i, const void* const data_v, const size_t data_len_i);
