#include "crypto_sign.h"
#include "sha512.h"
#include "ge.h"

int crypto_sign_keypair(unsigned char *pk, unsigned char *sk, unsigned char *seed)
{
  unsigned char h[64];
  ge_p3 A;
  int i;

  sha512(h,seed,32);
  h[0] &= 248;
  h[31] &= 63;
  h[31] |= 64;

  ge_scalarmult_base(&A,h);
  ge_p3_tobytes(pk,&A);

  for (i = 0;i < 32;++i) sk[i] = seed[i];
  for (i = 0;i < 32;++i) sk[32 + i] = pk[i];
  return 0;
}
