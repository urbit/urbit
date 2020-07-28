#include "urcrypt.h"

int
urcrypt_ed_scalarmult(uint8_t a[32], uint8_t b[32], uint8_t out[32])
{
  ge_p3 B, result;

  if ( ge_frombytes_negate_vartime(&B, b) != 0 ) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(B.X, B.X);
  fe_neg(B.T, B.T);

  ge_scalarmult(&result, a, &B);
  ge_p3_tobytes(out, &result);
  return 0;
}
