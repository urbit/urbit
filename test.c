
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ed25519.h"

char *msg = "Hello World";

int main(int argc, char *argv[]) {
    unsigned char sk[64], vk[32];
    unsigned char *sigmsg, *newmsg;
    unsigned long long sigmsglen, newmsglen;
    int ret;
    ed25519_sign_keypair(vk, sk, "0123456890123456789012");
    printf("got keypair\n");
    sigmsg = malloc(strlen(msg)+1+64);
    if (!sigmsg)
        return 1;
    ed25519_sign(sigmsg, &sigmsglen, (unsigned char *)msg, strlen(msg)+1, sk);
    printf("got signature\n");
    if (sigmsglen != strlen(msg)+1+64)
        return 2;
    newmsg = malloc(sigmsglen);
    if (!newmsg)
        return 3;
    ret = ed25519_sign_open(newmsg, &newmsglen, sigmsg, sigmsglen, vk);
    printf("verified signature\n");
    if (ret == 0)
        printf("good!\n");
    else
        printf("bad\n");
    sigmsg[0] ^= 0x01;
    ret = ed25519_sign_open(newmsg, &newmsglen, sigmsg, sigmsglen, vk);
    if (ret == 0) 
        printf("bad: failed to detect simple corruption\n");
    else
        printf("good: detected simple corruption\n");
    return 0;
}