#ifndef URCRYPT_H
#define URCRYPT_H

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ed25519.h>
#include <ge-additions.h>

int urcrypt_ed_point_add(uint8_t a[32], uint8_t b[32], uint8_t out[32]);
int urcrypt_ed_scalarmult(uint8_t a[32], uint8_t b[32], uint8_t out[32]);
void urcrypt_ed_scalarmult_base(uint8_t a[32], uint8_t out[32]);
int urcrypt_ed_add_scalarmult_scalarmult_base(uint8_t a[32],
                                              uint8_t a_point[32],
                                              uint8_t b[32],
                                              uint8_t out[32]);
int urcrypt_ed_add_double_scalarmult(uint8_t a[32],
                                     uint8_t a_point[32],
                                     uint8_t b[32],
                                     uint8_t b_point[32],
                                     uint8_t out[32]);
void urcrypt_ed_puck(uint8_t seed[32], uint8_t out[32]);
void urcrypt_ed_shar(uint8_t public[32], uint8_t seed[32], uint8_t out[32]);
void urcrypt_ed_sign(uint8_t *message,
                     size_t length,
                     uint8_t seed[32],
                     uint8_t out[64]);

bool urcrypt_ed_veri(uint8_t *message, size_t length,
                     uint8_t signature[64], uint8_t public[32]);
#endif
