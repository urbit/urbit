#include "urcrypt.h"

#include <string.h>
#include <ed25519.h>
#include <ge-additions.h>

#include <openssl/crypto.h>
#include <openssl/ripemd.h>
#include <openssl/sha.h>
#include <openssl/aes.h>
#include <aes_siv.h>

#include <argon2.h>
#include <blake2.h>

#include <secp256k1.h>
#include <secp256k1_recovery.h>

static urcrypt_malloc_t _urcrypt_ssl_malloc_ptr;
static urcrypt_realloc_t _urcrypt_ssl_realloc_ptr;
static urcrypt_free_t _urcrypt_ssl_free_ptr;

static void*
_urcrypt_malloc_ssl(size_t len
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { return (*_urcrypt_ssl_malloc_ptr)(len); }

static void*
_urcrypt_realloc_ssl(void* ptr, size_t len
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { return (*_urcrypt_ssl_realloc_ptr)(ptr, len); }

static void
_urcrypt_free_ssl(void* ptr
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { (*_urcrypt_ssl_free_ptr)(ptr); }

int
urcrypt_set_openssl_mem_functions(urcrypt_malloc_t malloc_ptr,
                                  urcrypt_realloc_t realloc_ptr,
                                  urcrypt_free_t free_ptr)
{
  if ( CRYPTO_set_mem_functions(&_urcrypt_malloc_ssl,
                                &_urcrypt_realloc_ssl,
                                &_urcrypt_free_ssl) ) {
    _urcrypt_ssl_malloc_ptr = malloc_ptr;
    _urcrypt_ssl_realloc_ptr = realloc_ptr;
    _urcrypt_ssl_free_ptr = free_ptr;
    return 0;
  }
  else {
    return -1;
  }
}

int
urcrypt_ed_point_add(const uint8_t a[32],
                     const uint8_t b[32],
                     uint8_t out[32])
{
  ge_p3 A, B;
  ge_cached b_cached;
  ge_p1p1 sum;
  ge_p3 result;

  if ( ge_frombytes_negate_vartime(&A, a) != 0 ) {
    return -1;
  }

  if ( ge_frombytes_negate_vartime(&B, b) != 0 ) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(A.X, A.X);
  fe_neg(A.T, A.T);
  fe_neg(B.X, B.X);
  fe_neg(B.T, B.T);

  ge_p3_to_cached(&b_cached, &B);
  ge_add(&sum, &A, &b_cached);
  ge_p1p1_to_p3(&result, &sum);

  ge_p3_tobytes(out, &result);

  return 0;
}

int
urcrypt_ed_scalarmult(const uint8_t a[32],
                      const uint8_t b[32],
                      uint8_t out[32])
{
  ge_p3 B, result;

  if ( ge_frombytes_negate_vartime(&B, b) != 0 ) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(B.X, B.X);
  fe_neg(B.T, B.T);

  ge_scalarmult(&result, a, &B);
  ge_p3_tobytes(out, &result);
  return 0;
}

void
urcrypt_ed_scalarmult_base(const uint8_t a[32],
                           uint8_t out[32])
{
  ge_p3 R;
  ge_scalarmult_base(&R, a);
  ge_p3_tobytes(out, &R);
}

int
urcrypt_ed_add_scalarmult_scalarmult_base(const uint8_t a[32],
                                          const uint8_t a_point[32],
                                          const uint8_t b[32],
                                          uint8_t out[32])
{
  ge_p2 r;
  ge_p3 A;

  if (ge_frombytes_negate_vartime(&A, a_point) != 0) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(A.X, A.X);
  fe_neg(A.T, A.T);

  ge_double_scalarmult_vartime(&r, a, &A, b);
  ge_tobytes(out, &r);

  return 0;
}

int
urcrypt_ed_add_double_scalarmult(const uint8_t a[32],
                                 const uint8_t a_point[32],
                                 const uint8_t b[32],
                                 const uint8_t b_point[32],
                                 uint8_t out[32])
{
  ge_p3 A, B, a_result, b_result, final_result;
  ge_cached b_result_cached;
  ge_p1p1 sum;

  if ( ge_frombytes_negate_vartime(&A, a_point) != 0 ) {
    return -1;
  }

  if ( ge_frombytes_negate_vartime(&B, b_point) != 0 ) {
    return -1;
  }

  // Undo the negation from above. See add_scalar.c in the ed25519 distro.
  fe_neg(A.X, A.X);
  fe_neg(A.T, A.T);
  fe_neg(B.X, B.X);
  fe_neg(B.T, B.T);

  // Perform the multiplications of a*A and b*B
  ge_scalarmult(&a_result, a, &A);
  ge_scalarmult(&b_result, b, &B);

  // Sum those two points
  ge_p3_to_cached(&b_result_cached, &b_result);
  ge_add(&sum, &a_result, &b_result_cached);

  ge_p1p1_to_p3(&final_result, &sum);
  ge_p3_tobytes(out, &final_result);

  return 0;
}

void
urcrypt_ed_puck(const uint8_t seed[32],
                uint8_t out[32])
{
  uint8_t secret[64];
  ed25519_create_keypair(out, secret, seed);
}

void
urcrypt_ed_shar(const uint8_t public[32],
                const uint8_t seed[32],
                uint8_t out[32])
{
  uint8_t self[32], exp[64];

  memset(self, 0, 32);
  memset(exp, 0, 64);
  memset(out, 0, 32);

  ed25519_create_keypair(self, exp, seed);
  ed25519_key_exchange(out, public, exp);
}

void
urcrypt_ed_sign(const uint8_t *message,
                size_t length,
                const uint8_t seed[32],
                uint8_t out[64])
{
  uint8_t public[64], secret[64];

  memset(public, 0, 64);
  memset(secret, 0, 64);
  memset(out, 0, 64);

  ed25519_create_keypair(public, secret, seed);
  ed25519_sign(out, message, length, public, secret);
}

bool
urcrypt_ed_veri(const uint8_t *message,
                size_t length,
                const uint8_t public[32],
                const uint8_t signature[64])
{
  return ( ed25519_verify(signature, message, length, public) == 1 )
    ? true
    : false;
}

static void
_urcrypt_reverse(size_t size, uint8_t *ptr) {
  if ( size > 0 ) {
    size_t i, j;
    uint8_t tmp;
    for ( i = 0, j = size - 1; i < j; i++, j-- ) {
      tmp = ptr[i];
      ptr[i] = ptr[j];
      ptr[j] = tmp;
    }
  }
}

int
urcrypt_aes_ecba_en(uint8_t key[16], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(16, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecba_de(uint8_t key[16], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(16, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_en(uint8_t key[24], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(24, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_de(uint8_t key[24], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(24, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_en(uint8_t key[32], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(32, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_de(uint8_t key[32], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  _urcrypt_reverse(32, key);
  _urcrypt_reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse(16, out);
    return 0;
  }
}

int
_urcrypt_cbc_pad(uint8_t **message_ptr,
                 size_t *length_ptr,
                 urcrypt_realloc_t realloc_ptr)
{
  size_t length = *length_ptr,
         remain = length % 16;

  if ( 0 == remain ) {
    // no padding needed
    return 0;
  }
  else {
    size_t padding = 16 - remain,
           padded  = length + padding;

    if ( padded < length ) {
      // size_t overflow
      return -1;
    }
    else {
      uint8_t *out = (*realloc_ptr)(*message_ptr, padded);
      if ( NULL == out ) {
        return -2;
      }
      else {
        memset(out + length, 0, padding);
        *message_ptr = out;
        *length_ptr  = padded;
        return 0;
      }
    }
  }
}

int
_urcrypt_cbc_help(uint8_t **message_ptr,
                  size_t *length_ptr,
                  const AES_KEY *key,
                  uint8_t ivec[16],
                  const int enc,
                  urcrypt_realloc_t realloc_ptr)
{
  if ( 0 != _urcrypt_cbc_pad(message_ptr, length_ptr, realloc_ptr) ) {
    return -1;
  }
  else {
    uint8_t *out = *message_ptr;
    size_t length = *length_ptr;
    _urcrypt_reverse(16, ivec);
    _urcrypt_reverse(length, out);
    AES_cbc_encrypt(out, out, length, key, ivec, enc);
    _urcrypt_reverse(length, out);
    return 0;
  }
}

int
urcrypt_aes_cbca_en(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[16],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(16, key);

  if ( 0 != AES_set_encrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_ENCRYPT, realloc_ptr);
  }
}

int
urcrypt_aes_cbca_de(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[16],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(16, key);

  if ( 0 != AES_set_decrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_DECRYPT, realloc_ptr);
  }
}

int
urcrypt_aes_cbcb_en(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[24],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(24, key);

  if ( 0 != AES_set_encrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_ENCRYPT, realloc_ptr);
  }
}

int
urcrypt_aes_cbcb_de(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[24],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(24, key);

  if ( 0 != AES_set_decrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_DECRYPT, realloc_ptr);
  }
}

int
urcrypt_aes_cbcc_en(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[32],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(32, key);

  if ( 0 != AES_set_encrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_ENCRYPT, realloc_ptr);
  }
}

int
urcrypt_aes_cbcc_de(uint8_t **message_ptr,
                    size_t *length_ptr,
                    uint8_t key[32],
                    uint8_t ivec[16],
                    urcrypt_realloc_t realloc_ptr)
{
  AES_KEY aes_key;

  _urcrypt_reverse(32, key);

  if ( 0 != AES_set_decrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    return _urcrypt_cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_DECRYPT, realloc_ptr);
  }
}

/* FIXME TODO remove
#include <stdarg.h>
void dbg(const char* fmt, ...)
{
  va_list ap;
  FILE *nukes = fopen("/tmp/urcrypt.txt", "a");
  va_start(ap, fmt);
  vfprintf(nukes, fmt, ap);
  va_end(ap);
  fclose(nukes);
}
*/

static AES_SIV_CTX*
_urcrypt_aes_siv_init(uint8_t *key,
                      size_t key_length,
                      urcrypt_aes_siv_data *data,
                      size_t data_length)
{
  AES_SIV_CTX *ctx = AES_SIV_CTX_new();
  if ( NULL == ctx ) {
    return NULL;
  }
  else {
    _urcrypt_reverse(key_length, key);
    if ( 0 == AES_SIV_Init(ctx, key, key_length) ) {
      AES_SIV_CTX_free(ctx);
      return NULL;
    }
    else {
      size_t i, len;
      uint8_t *dat;

      for ( i = 0; i < data_length; ++i ) {
        len = data[i].length;
        dat = data[i].bytes;
        _urcrypt_reverse(len, dat);
        if ( 0 == AES_SIV_AssociateData(ctx, dat, len) ) {
          AES_SIV_CTX_free(ctx);
          return NULL;
        }
      }

      return ctx;
    }
  }
}

static int
_urcrypt_aes_siv_en(uint8_t *key,
                    size_t key_length,
                    uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t iv[16],
                    uint8_t *out)
{
  AES_SIV_CTX *ctx = _urcrypt_aes_siv_init(key, key_length, data, data_length);

  if ( NULL == ctx ) {
    return -1;
  }
  else {
    int ret;
    _urcrypt_reverse(message_length, message);
    ret = AES_SIV_EncryptFinal(ctx, iv, out, message, message_length);
    AES_SIV_CTX_free(ctx);

    if ( 0 == ret ) {
      return -2;
    }
    else {
      _urcrypt_reverse(16, iv);
      _urcrypt_reverse(message_length, out);
      return 0;
    }
  }
}

int
_urcrypt_aes_siv_de(uint8_t *key,
                    size_t key_length,
                    uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t iv[16],
                    uint8_t *out)
{
  AES_SIV_CTX *ctx = _urcrypt_aes_siv_init(key, key_length, data, data_length);

  if ( NULL == ctx ) {
    return -1;
  }
  else {
    int ret;

    _urcrypt_reverse(message_length, message);
    _urcrypt_reverse(16, iv);
    ret = AES_SIV_DecryptFinal(ctx, out, iv, message, message_length);
    AES_SIV_CTX_free(ctx);

    if ( 0 == ret ) {
      return -2;
    }
    else {
      _urcrypt_reverse(message_length, out);
      return 0;
    }
  }
}

int
urcrypt_aes_siva_en(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[32],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_en(key, 32,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_aes_siva_de(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[32],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_de(key, 32,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_aes_sivb_en(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[48],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_en(key, 48,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_aes_sivb_de(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[48],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_de(key, 48,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_aes_sivc_en(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[64],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_en(key, 64,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_aes_sivc_de(uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t key[64],
                    uint8_t iv[16],
                    uint8_t *out)
{
  return _urcrypt_aes_siv_de(key, 64,
      message, message_length, data, data_length, iv, out);
}

int
urcrypt_ripemd160(uint8_t *message, size_t length, uint8_t out[20])
{
  unsigned long n = length;

  if ( length != n ) {
    return -1;
  }
  else {
    _urcrypt_reverse(length, message);
    RIPEMD160(message, n, out);
    _urcrypt_reverse(20, out);
    return 0;
  }
}

void
urcrypt_sha1(uint8_t *message, size_t length, uint8_t out[20])
{
  _urcrypt_reverse(length, message);
  SHA1(message, length, out);
  _urcrypt_reverse(20, out);
}

void
urcrypt_shay(const uint8_t *message, size_t length, uint8_t out[32])
{
  SHA256(message, length, out);
}

void
urcrypt_shal(const uint8_t *message, size_t length, uint8_t out[64])
{
  SHA512(message, length, out);
}

void
urcrypt_shas(uint8_t *salt, size_t salt_length,
             const uint8_t *message, size_t message_length,
             uint8_t out[32])
{
  size_t i;
  uint8_t mid[32];

  // docs don't say what happens if msg overlaps with out
  urcrypt_shay(message, message_length, mid);

  if ( salt_length > 32 ) {
    for ( i = 0; i < 32; i++ ) {
      salt[i] ^= mid[i];
    }
    urcrypt_shay(salt, salt_length, out);
  }
  else {
    for ( i = 0; i < salt_length; i++ ) {
      mid[i] ^= salt[i];
    }
    urcrypt_shay(mid, 32, out);
  }
}

/* argon2 does memory allocation, but takes function pointers in the context.
 * the signatures don't match, so we need these wrappers.
 */
static urcrypt_malloc_t _urcrypt_argon2_malloc_ptr;
static urcrypt_free_t _urcrypt_argon2_free_ptr;

static int
_urcrypt_argon2_alloc(uint8_t** output, size_t bytes)
{
  *output = (*_urcrypt_argon2_malloc_ptr)(bytes);
  return (NULL != *output);
}

static void
_urcrypt_argon2_free(uint8_t* memory, size_t bytes)
{
  (*_urcrypt_argon2_free_ptr)(memory);
}

// library convention is to have sizes in size_t, but argon2 wants them
// in uint32_t, so here's a helper macro for ensuring equivalence.
#define SZ_32(s) ( sizeof(size_t) <= sizeof(uint32_t) || s <= 0xFFFFFFFF )

const char*
urcrypt_argon2(urcrypt_argon2_type type,
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
               urcrypt_free_t free_ptr)
{
  if ( !( SZ_32(secret_length) &&
          SZ_32(associated_length) &&
          SZ_32(password_length) &&
          SZ_32(salt_length) &&
          SZ_32(out_length) ) ) {
    return "length > 32 bits";
  }
  else {
    int (*f)(argon2_context*);
    int result;

    switch ( type ) {
      default:
        return "unknown type";
      case urcrypt_argon2_d:
        f = &argon2d_ctx;
        break;
      case urcrypt_argon2_i:
        f = &argon2i_ctx;
        break;
      case urcrypt_argon2_id:
        f = &argon2id_ctx;
        break;
      case urcrypt_argon2_u:
        f = &argon2u_ctx;
        break;
    }

    _urcrypt_reverse(secret_length, secret);
    _urcrypt_reverse(associated_length, associated);
    _urcrypt_reverse(password_length, password);
    _urcrypt_reverse(salt_length, salt);

    argon2_context context = {
      out,                   // output array, at least [digest length] in size
      out_length,            // digest length
      password,              // password array
      password_length,       // password length
      salt,                  // salt array
      salt_length,           // salt length
      secret,                // optional secret data
      secret_length,
      associated,            // optional associated data
      associated_length,
      time_cost,             // performance cost configuration
      memory_cost,
      threads,
      threads,
      version,               // algorithm version
      &_urcrypt_argon2_alloc,// custom memory allocation function
      &_urcrypt_argon2_free, // custom memory deallocation function
      ARGON2_DEFAULT_FLAGS   // by default only internal memory is cleared
    };

    _urcrypt_argon2_malloc_ptr = malloc_ptr;
    _urcrypt_argon2_free_ptr = free_ptr;
    result = (*f)(&context);

    if ( ARGON2_OK != result ) {
      return argon2_error_message(result);
    }
    else {
      _urcrypt_reverse(out_length, out);
      return NULL;
    }
  }
}

int
urcrypt_blake2(size_t message_length,
               uint8_t *message,
               size_t key_length,
               uint8_t key[64],
               size_t out_length,
               uint8_t *out)
{
  if ( key_length > 64 ) {
    return -1;
  }
  else {
    _urcrypt_reverse(message_length, message);
    _urcrypt_reverse(key_length, key);

    if ( 0 != blake2b(out, out_length,
                      message, message_length,
                      key, key_length)) {
      return -1;
    }
    else {
      _urcrypt_reverse(out_length, out);
      return 0;
    }
  }
}

int
urcrypt_secp_make(uint8_t hash[32], uint8_t key[32], uint8_t out[32])
{
  _urcrypt_reverse(32, hash);
  _urcrypt_reverse(32, key);

  if ( 1 != secp256k1_nonce_function_rfc6979(
    out,   // OUT: return arg for nonce
    hash,  // IN: message / hash */
    key,   // IN: key32
    NULL,  // IN: algorithm (NULL == ECDSA)
    NULL,  // IN: arbitrary data pointer (unused)
    0) ) { // IN: attempt number (0 == normal)
    return -1;
  }
  else {
    _urcrypt_reverse(32, out);
    return 0;
  }
}
