#ifndef URCRYPT_H
#define URCRYPT_H

#include <stdint.h>
#include <ed25519.h>
#include <ge-additions.h>

int urcrypt_ed_scalarmult(uint8_t a[32], uint8_t b[32], uint8_t out[32]);

#endif
