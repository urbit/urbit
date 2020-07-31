#include "urcrypt.h"

int
urcrypt_ed_point_add(uint8_t a[32], uint8_t b[32], uint8_t out[32])
{
  ge_p3 A, B;
  ge_cached b_cached;
  ge_p1p1 sum;
  ge_p3 result;

  if ( ge_frombytes_negate_vartime(&A, a) != 0 ) {
    return -1;
  }

  if ( ge_frombytes_negate_vartime(&B, b) != 0 ) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(A.X, A.X);
  fe_neg(A.T, A.T);
  fe_neg(B.X, B.X);
  fe_neg(B.T, B.T);

  ge_p3_to_cached(&b_cached, &B);
  ge_add(&sum, &A, &b_cached);
  ge_p1p1_to_p3(&result, &sum);

  ge_p3_tobytes(out, &result);

  return 0;
}

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

void
urcrypt_ed_scalarmult_base(uint8_t a[32], uint8_t out[32])
{
  ge_p3 R;
  ge_scalarmult_base(&R, a);
  ge_p3_tobytes(out, &R);
}

void
urcrypt_ed_sign(uint8_t *message,
                size_t length,
                uint8_t seed[32],
                uint8_t out[64])
{
  uint8_t public[64], secret[64];

  memset(public, 0, 64);
  memset(secret, 0, 64);
  memset(out, 0, 64);

  ed25519_create_keypair(public, secret, seed);
  ed25519_sign(out, message, length, public, secret);
}
