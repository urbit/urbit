#include "ed25519.h"
#include "sha512.h"
#include "ge.h"
#include "sc.h"


int ed25519_sign(unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *sign_key) {
    unsigned char az[64];
    unsigned char r[64];
    unsigned char hram[64];
    ge_p3 R;
    sha512_context hash;

    sha512(sign_key, 32, az);
    az[0] &= 248;
    az[31] &= 63;
    az[31] |= 64;

    sha512_init(&hash);
    sha512_update(&hash, az + 32, 32);
    sha512_update(&hash, message, message_len);
    sha512_final(&hash, r);

    sc_reduce(r);
    ge_scalarmult_base(&R, r);
    ge_p3_tobytes(signature, &R);

    sha512_init(&hash);
    sha512_update(&hash, signature, 32);
    sha512_update(&hash, sign_key + 32, 32);
    sha512_update(&hash, message, message_len);
    sha512_final(&hash, hram);

    sc_reduce(hram);
    sc_muladd(signature + 32, hram, az, r);
    
    return 0;
}
