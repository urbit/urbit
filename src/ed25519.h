#ifndef ED25519_H
#define ED25519_H

#include <stddef.h>

int ed25519_sign(unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *sign_key);
int ed25519_verify(const unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *verify_key);
int ed25519_create_keypair(unsigned char *verify_key, unsigned char *sign_key, unsigned char *seed);

#ifndef ED25519_NO_SEED
int ed25519_create_seed(unsigned char *seed);
#endif

#endif