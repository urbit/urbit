#include "urcrypt.h"
#include "util.h"
#include <keccak-tiny.h>

#define defkec(bits,byts) \
  int urcrypt_keccak_##bits(const uint8_t *message, size_t length, \
                            uint8_t out[byts]) \
  { \
    if ( 0 == keccak_##bits(out, byts, message, length) ) { \
      urcrypt__reverse(byts, out); \
      return 0; \
    } \
    else { \
      return -1; \
    }\
  }

defkec(224, 28)
defkec(256, 32)
defkec(384, 48)
defkec(512, 64)
