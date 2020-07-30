#ifndef URCRYPT_H
#define URCRYPT_H

#include <stdint.h>
#include <string.h>
#include <ed25519.h>
#include <ge-additions.h>

int urcrypt_ed_scalarmult(uint8_t a[32], uint8_t b[32], uint8_t out[32]);
void urcrypt_ed_sign(uint8_t *message,
                     size_t length,
                     uint8_t seed[32],
                     uint8_t signature[64]);

#endif
