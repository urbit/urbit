#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ed25519.h"
#include <time.h>
char msg[] = "Hello World";

int main(int argc, char *argv[]) {
    unsigned char sk[64], vk[32], seed[32];
    unsigned char *sigmsg;
	FILE *f;
	int ret;

    ed25519_create_seed(seed);

    f = fopen("seed.txt", "wb");
    fwrite(seed, 32, 1, f);
    fclose(f);

    ed25519_create_keypair(vk, sk, "01234567890123456789012345678901");


    printf("got keypair\n");
    sigmsg = malloc(64);
    if (!sigmsg)
        return 1;
    ed25519_sign(sigmsg, (unsigned char *)msg, strlen(msg), sk);
    printf("got signature\n");

	f = fopen("sig.txt", "wb");
	fwrite(sigmsg, 64, 1, f);
	fclose(f);

    ret = ed25519_verify(sigmsg, "Hello World", strlen(msg), vk);
    printf("verified signature\n");
    if (ret == 0)
        printf("good!\n");
    else
        printf("bad\n");
    sigmsg[0] ^= 0x01;
    ret = ed25519_verify(sigmsg, msg, strlen(msg), vk);
    if (ret == 0) 
        printf("bad: failed to detect simple corruption\n");
    else
        printf("good: detected simple corruption\n");

    return 0;
}