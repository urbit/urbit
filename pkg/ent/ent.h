#pragma once

#include <stddef.h>

/*
  Fills buf with high-quality entropy.

  buflen is the number of bytes, no greater than 256.

  Returns 0 on success. On failure, returns -1 and sets errno to
  indicate the error.
*/
int ent_getentropy(void* buf, size_t buflen);
