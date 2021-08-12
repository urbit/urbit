#include "urcrypt.h"

void
urcrypt__reverse(size_t size, uint8_t *ptr) {
  if ( size > 0 ) {
    size_t i, j;
    uint8_t tmp;
    for ( i = 0, j = size - 1; i < j; i++, j-- ) {
      tmp = ptr[i];
      ptr[i] = ptr[j];
      ptr[j] = tmp;
    }
  }
}
