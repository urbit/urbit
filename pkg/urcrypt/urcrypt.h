#ifndef URCRYPT_H
#define URCRYPT_H

// XX most of these should be moved to urcrypt.c

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <ed25519.h>
#include <ge-additions.h>

#include <openssl/crypto.h>
#include <openssl/aes.h>

#include <argon2.h>
#include <blake2.h>

typedef void *(*urcrypt_malloc_t)(size_t);
typedef void *(*urcrypt_realloc_t)(void*, size_t);
typedef void (*urcrypt_free_t)(void*);

/* We depend on OpenSSL for various reasons, which doesn't promise not to
 * allocate memory and has the annoying CRYPTO_set_mem_functions api. We
 * are therefore forced to support it in some fashion.
 *
 * If you need to control urcrypt's internal OpenSSL allocation, you can call
 * this function. It just wraps the OpenSSL function.
 *
 * urcrypt will not use these functions directly.
 */
int urcrypt_set_openssl_functions(urcrypt_malloc_t malloc_ptr,
                                  urcrypt_realloc_t realloc_ptr,
                                  urcrypt_free_t free_ptr);

// const arguments are not written to, non-const arguments may be
// array sizes[64] are purely documentary

// 0 on success, result in out
int urcrypt_ed_point_add(const uint8_t a[32],
                         const uint8_t b[32],
                         uint8_t out[32]);
int urcrypt_ed_scalarmult(const uint8_t a[32],
                          const uint8_t b[32],
                          uint8_t out[32]);
// void functions have no failure mode
void urcrypt_ed_scalarmult_base(const uint8_t a[32],
                                uint8_t out[32]);
int urcrypt_ed_add_scalarmult_scalarmult_base(const uint8_t a[32],
                                              const uint8_t a_point[32],
                                              const uint8_t b[32],
                                              uint8_t out[32]);
int urcrypt_ed_add_double_scalarmult(const uint8_t a[32],
                                     const uint8_t a_point[32],
                                     const uint8_t b[32],
                                     const uint8_t b_point[32],
                                     uint8_t out[32]);

void urcrypt_ed_puck(const uint8_t seed[32],
                     uint8_t out[32]);
void urcrypt_ed_shar(const uint8_t public[32],
                     const uint8_t seed[32],
                     uint8_t out[32]);
void urcrypt_ed_sign(const uint8_t *message,
                     size_t length,
                     const uint8_t seed[32],
                     uint8_t out[64]);
// return value means the signature was (not) verified
bool urcrypt_ed_veri(const uint8_t *message,
                     size_t length,
                     const uint8_t signature[64],
                     const uint8_t public[32]);

int urcrypt_aes_ecba_en(uint8_t key[16], uint8_t block[16], uint8_t out[16]);
int urcrypt_aes_ecba_de(uint8_t key[16], uint8_t block[16], uint8_t out[16]);
int urcrypt_aes_ecbb_en(uint8_t key[24], uint8_t block[16], uint8_t out[16]);
int urcrypt_aes_ecbb_de(uint8_t key[24], uint8_t block[16], uint8_t out[16]);
int urcrypt_aes_ecbc_en(uint8_t key[32], uint8_t block[16], uint8_t out[16]);
int urcrypt_aes_ecbc_de(uint8_t key[32], uint8_t block[16], uint8_t out[16]);

// message and length are read/write so
// realloc_ptr can be used as realloc to pad message
int urcrypt_aes_cbca_en(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[16],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);
int urcrypt_aes_cbca_de(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[16],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);
int urcrypt_aes_cbcb_en(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[24],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);
int urcrypt_aes_cbcb_de(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[24],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);
int urcrypt_aes_cbcc_en(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[32],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);
int urcrypt_aes_cbcc_de(uint8_t **message_ptr,
                        size_t *length_ptr,
                        uint8_t key[32],
                        uint8_t ivec[16],
                        urcrypt_realloc_t realloc_ptr);

typedef enum urcrypt_argon2_type {
  urcrypt_argon2_d  = 0,
  urcrypt_argon2_i  = 1,
  urcrypt_argon2_id = 2,
  urcrypt_argon2_u  = 10,
} urcrypt_argon2_type;

/* returns a constant error message string or NULL for success */
const char* urcrypt_argon2(urcrypt_argon2_type type,
                           uint32_t version,
                           uint32_t threads,
                           uint32_t memory_cost,
                           uint32_t time_cost,
                           size_t secret_length,
                           uint8_t *secret,
                           size_t associated_length,
                           uint8_t *associated,
                           size_t password_length,
                           uint8_t *password,
                           size_t salt_length,
                           uint8_t *salt,
                           size_t out_length,
                           uint8_t *out,
                           urcrypt_malloc_t malloc_ptr,
                           urcrypt_free_t free_ptr);

int urcrypt_blake2(size_t message_length,
                   uint8_t *message,
                   size_t key_length,
                   uint8_t key[64],
                   size_t out_length,
                   uint8_t *out);

#endif
