#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "ed25519.h"

const char message[] = "Hello, world!";

int main(int argc, char *argv[]) {
    unsigned char sign_key[64], verify_key[32], seed[32];
    unsigned char signature[64];

	clock_t start;
	clock_t end;
	int i;

    /* create a random seed, and a keypair out of that seed */
    ed25519_create_seed(seed);
    ed25519_create_keypair(verify_key, sign_key, seed);

    /* create signature on the message with the sign key */
    ed25519_sign(signature, message, strlen(message), sign_key);

    /* verify the signature */
    if (ed25519_verify(signature, message, strlen(message), verify_key)) {
        printf("invalid signature\n");
    } else {
        printf("valid signature\n");
    }

    /* make a slight adjustment and verify again */
    signature[44] ^= 0x10;
    if (ed25519_verify(signature, message, strlen(message), verify_key)) {
        printf("correctly detected signature change\n");
    } else {
        printf("incorrectly accepted signature change\n");
    }

    /* test performance */
    printf("testing sign performance: ");
    start = clock();
    for (i = 0; i < 10000; ++i) {
        ed25519_sign(signature, message, strlen(message), sign_key);
    }
    end = clock();

    printf("%fus per signature\n", ((double) ((end - start) * 1000)) / CLOCKS_PER_SEC / i * 1000);
    printf("%f\n", (double) (end - start) / CLOCKS_PER_SEC);

    printf("testing verify performance: ");
    start = clock();
    for (i = 0; i < 10000; ++i) {
        ed25519_verify(signature, message, strlen(message), verify_key);
    }
    end = clock();

    printf("%fus per signature\n", ((double) ((end - start) * 1000)) / CLOCKS_PER_SEC / i * 1000);

    return 0;
}