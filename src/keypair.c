#include "ed25519.h"
#include "sha512.h"
#include "ge.h"



int ed25519_create_keypair(unsigned char *verify_key, unsigned char *sign_key, unsigned char *seed) {
    unsigned char h[64];
    ge_p3 A;
    int i;
    sha512(seed, 32, h);
    h[0] &= 248;
    h[31] &= 63;
    h[31] |= 64;
    ge_scalarmult_base(&A, h);
    ge_p3_tobytes(verify_key, &A);

    for (i = 0; i < 32; ++i) {
        sign_key[i] = seed[i];
    }

    for (i = 0; i < 32; ++i) {
        sign_key[32 + i] = verify_key[i];
    }

    return 0;
}
