#ifndef URCRYPT_H
#define URCRYPT_H
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
// const arguments are not written to, non-const arguments may be
// all arrays are in little-endian byte order.
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

typedef void* (*urcrypt_realloc_t)(void*, size_t);

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

typedef struct {
  size_t length;
  uint8_t *bytes;
} urcrypt_aes_siv_data;

int urcrypt_aes_siva_en(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[32],
                        uint8_t iv[16],
                        uint8_t *out);
int urcrypt_aes_siva_de(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[32],
                        uint8_t iv[16],
                        uint8_t *out);
int urcrypt_aes_sivb_en(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[48],
                        uint8_t iv[16],
                        uint8_t *out);
int urcrypt_aes_sivb_de(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[48],
                        uint8_t iv[16],
                        uint8_t *out);
int urcrypt_aes_sivc_en(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[64],
                        uint8_t iv[16],
                        uint8_t *out);
int urcrypt_aes_sivc_de(uint8_t *message,
                        size_t message_length,
                        urcrypt_aes_siv_data *data,
                        size_t data_length,
                        uint8_t key[64],
                        uint8_t iv[16],
                        uint8_t *out);

int urcrypt_ripemd160(uint8_t *message, size_t length, uint8_t out[20]);

void urcrypt_sha1(uint8_t *message, size_t length, uint8_t out[20]);
void urcrypt_shay(const uint8_t *message, size_t length, uint8_t out[32]);
void urcrypt_shal(const uint8_t *message, size_t length, uint8_t out[64]);
void urcrypt_shas(uint8_t *salt, size_t salt_length,
                  const uint8_t *message, size_t message_length,
                  uint8_t out[32]);

typedef int (*urcrypt_argon2_alloc_t)(uint8_t**, size_t);
typedef void (*urcrypt_argon2_free_t)(uint8_t*, size_t);

#define urcrypt_argon2_d  0
#define urcrypt_argon2_i  1
#define urcrypt_argon2_id 2
#define urcrypt_argon2_u  10

/* returns a constant error message string or NULL for success */
const char* urcrypt_argon2(uint8_t  type,  // one of the urcrpyt_argon2_*
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
                           urcrypt_argon2_alloc_t alloc_ptr,
                           urcrypt_argon2_free_t free_ptr);

int urcrypt_blake2(size_t message_length,
                   uint8_t *message,
                   size_t key_length,
                   uint8_t key[64],
                   size_t out_length,
                   uint8_t *out);

/* there is some long-term context associated with the secp library
 * (precomputed tables, etc), so secp functions require a context object
 */
typedef struct urcrypt_secp_context_struct urcrypt_secp_context;

// size of opaque secp handle, malloc and pass to init
size_t urcrypt_secp_prealloc_size(void);
// call this once at per context with high quality entropy
int urcrypt_secp_init(urcrypt_secp_context *context,
                      uint8_t entropy[32]);
// call before freeing opaque secp handle
void urcrypt_secp_destroy(urcrypt_secp_context *context);

int urcrypt_secp_make(uint8_t hash[32], uint8_t key[32], uint8_t out[32]);
int urcrypt_secp_sign(urcrypt_secp_context* context,
                      uint8_t hash[32],
                      uint8_t key[32],
                      uint8_t* out_v,
                      uint8_t out_r[32],
                      uint8_t out_s[32]);
int urcrypt_secp_reco(urcrypt_secp_context* context,
                      uint8_t hash[32],
                      uint8_t key_v, // 0, 1, 2, 3
                      const uint8_t key_r[32],
                      const uint8_t key_s[32],
                      uint8_t out_x[32],
                      uint8_t out_y[32]);

void urcrypt_scrypt_pbkdf_sha256(const uint8_t *passwd,
                                 size_t passwdlen,
                                 const uint8_t *salt,
                                 size_t saltlen,
                                 uint64_t count,
                                 size_t outlen, // must be at most 32*(2^32-1)
                                 uint8_t *out);

int urcrypt_scrypt(const uint8_t *passwd,
                   size_t passwdlen,
                   const uint8_t *salt,
                   size_t saltlen,
                   uint64_t n,
                   uint32_t r,
                   uint32_t p,
                   size_t outlen,
                   uint8_t *out);
#endif
