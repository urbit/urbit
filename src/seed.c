#ifndef ED25519_NO_SEED

#ifdef _WIN32
#include <Windows.h>
#include <Wincrypt.h>
#else
#include <stdio.h>
#endif

int ed25519_create_seed(unsigned char *seed) {
#ifdef _WIN32
    int i;
    HCRYPTPROV hCryptProv;

    if (!CryptAcquireContext(&hCryptProv, NULL, NULL, PROV_RSA_FULL | CRYPT_VERIFYCONTEXT, 0)) {
        return 1;
    }

    CryptGenRandom(hCryptProv, 32, seed);
    CryptReleaseContext(hCryptProv, 0);
#else
#endif
    return 0;
}

#endif