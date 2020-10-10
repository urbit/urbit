#include "urcrypt.h"
#include "util.h"
#include <openssl/ripemd.h>

int
urcrypt_ripemd160(uint8_t *message, size_t length, uint8_t out[20])
{
  unsigned long n = length;

  if ( length != n ) {
    return -1;
  }
  else {
    urcrypt__reverse(length, message);
    RIPEMD160(message, n, out);
    urcrypt__reverse(20, out);
    return 0;
  }
}

