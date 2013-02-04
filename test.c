#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define ED25519_DLL
#include "src/ed25519.h"

const char message[] = "Hello, world!";

int main(int argc, char *argv[]) {
    unsigned char public_key[32], private_key[64], seed[32];
    unsigned char signature[64];

	clock_t start;
	clock_t end;
	int i;

    /* create a random seed, and a keypair out of that seed */
    ed25519_create_seed(seed);
    ed25519_create_keypair(public_key, private_key, seed);

    /* create signature on the message with the keypair */
    ed25519_sign(signature, message, strlen(message), public_key, private_key);

    /* verify the signature */
    if (ed25519_verify(signature, message, strlen(message), public_key)) {
        printf("valid signature\n");
    } else {
        printf("invalid signature\n");
    }

    /* make a slight adjustment and verify again */
    signature[44] ^= 0x10;
    if (ed25519_verify(signature, message, strlen(message), public_key)) {
        printf("did not detect signature change\n");
    } else {
        printf("correctly detected signature change\n");
    }

    /* test performance */
    printf("testing key generation performance: ");
    start = clock();
    for (i = 0; i < 10000; ++i) {
        ed25519_create_seed(seed);
        ed25519_create_keypair(public_key, private_key, seed);
    }
    end = clock();

    printf("%fus per seed and keypair\n", ((double) ((end - start) * 1000)) / CLOCKS_PER_SEC / i * 1000);

    printf("testing sign performance: ");
    start = clock();
    for (i = 0; i < 10000; ++i) {
        ed25519_sign(signature, message, strlen(message), public_key, private_key);
    }
    end = clock();

    printf("%fus per signature\n", ((double) ((end - start) * 1000)) / CLOCKS_PER_SEC / i * 1000);

    printf("testing verify performance: ");
    start = clock();
    for (i = 0; i < 10000; ++i) {
        ed25519_verify(signature, message, strlen(message), public_key);
    }
    end = clock();

    printf("%fus per signature\n", ((double) ((end - start) * 1000)) / CLOCKS_PER_SEC / i * 1000);

    return 0;
}
