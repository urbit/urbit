#include "urcrypt.h"

static bool initialized = false;
static urcrypt_malloc_t urcrypt_malloc_ptr;
static urcrypt_realloc_t urcrypt_realloc_ptr;
static urcrypt_free_t urcrypt_free_ptr;

void* urcrypt_malloc(size_t len)
{
  return (*urcrypt_malloc_ptr)(len);
}

void* urcrypt_realloc(void *ptr, size_t len)
{
  return (*urcrypt_realloc_ptr)(ptr, len);
}

void urcrypt_free(void *ptr)
{
  (*urcrypt_free_ptr)(ptr);
}

static void*
urcrypt_malloc_ssl(size_t len
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { return urcrypt_malloc(len); }

static void*
urcrypt_realloc_ssl(void* ptr, size_t len
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { return urcrypt_realloc(ptr, len); }

static void
urcrypt_free_ssl(void* ptr
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
               , const char* file, int line
#endif
) { urcrypt_free(ptr); }

/* IMPORTANT: it is an error (undefined behavior) to call functions in
 * this library without first calling urcrypt_init() exactly once.
 */
int
urcrypt_init(urcrypt_malloc_t m, urcrypt_realloc_t r, urcrypt_free_t f)
{
  if ( initialized ) {
    return -1;
  }
  else {
    initialized = true;
    urcrypt_malloc_ptr  = ( NULL == m ) ? &malloc  : m;
    urcrypt_realloc_ptr = ( NULL == r ) ? &realloc : r;
    urcrypt_free_ptr    = ( NULL == f ) ? &free    : f;

    if ( CRYPTO_set_mem_functions(&urcrypt_malloc_ssl,
                                  &urcrypt_realloc_ssl,
                                  &urcrypt_free_ssl) ) {
      return 0;
    }
    else {
      return -2;
    }
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
_urcrypt_reverse_copy(size_t size, const uint8_t *in, uint8_t *out) {
  size_t i, j;
  for ( i = 0, j = size - 1; i < size; i++, j-- ) {
    out[i] = in[j];
  }
}

static void
_urcrypt_reverse_inplace(size_t size, uint8_t *ptr) {
  size_t i, j;
  uint8_t tmp;
  for ( i = 0, j = size - 1; i < j; i++, j-- ) {
    tmp = ptr[i];
    ptr[i] = ptr[j];
    ptr[j] = tmp;
  }
}

int
urcrypt_aes_ecba_en(const uint8_t key[16],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[16], rblock[16];

  _urcrypt_reverse_copy(16, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_encrypt_key(rkey, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecba_de(const uint8_t key[16],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[16], rblock[16];

  _urcrypt_reverse_copy(16, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_decrypt_key(rkey, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_en(const uint8_t key[24],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[24], rblock[16];

  _urcrypt_reverse_copy(24, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_encrypt_key(rkey, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_de(const uint8_t key[24],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[24], rblock[16];

  _urcrypt_reverse_copy(24, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_decrypt_key(rkey, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_en(const uint8_t key[32],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[32], rblock[16];

  _urcrypt_reverse_copy(32, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_encrypt_key(rkey, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_ENCRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_de(const uint8_t key[32],
                    const uint8_t block[16],
                    uint8_t out[16])
{
  AES_KEY aes_key;
  uint8_t rkey[32], rblock[16];

  _urcrypt_reverse_copy(32, key, rkey);
  _urcrypt_reverse_copy(16, block, rblock);

  if ( 0 != AES_set_decrypt_key(rkey, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(rblock, out, &aes_key, AES_DECRYPT);
    _urcrypt_reverse_inplace(16, out);
    return 0;
  }
}

static uint8_t*
_urcrypt_cbc_pad(size_t *length_ptr, const uint8_t *message)
{
  size_t length  = *length_ptr,
         padding = 16 - (length % 16),
         padded  = length + padding;
  uint8_t *buf   = urcrypt_malloc(padded);

  memset(buf, 0, padding);
  _urcrypt_reverse_copy(length, message, buf + padding);

  *length_ptr = padded;
  return buf;
}

uint8_t*
urcrypt_aes_cbca_en(const uint8_t *message,
                    size_t length,
                    const uint8_t key[16],
                    const uint8_t ivec[16],
                    size_t *out_length)
{
  AES_KEY aes_key;
  uint8_t rkey[16];

  _urcrypt_reverse_copy(16, key, rkey);

  if ( 0 != AES_set_encrypt_key(rkey, 128, &aes_key) ) {
    return NULL;
  }
  else {
    uint8_t riv[16], *in, *out;

    _urcrypt_reverse_copy(16, ivec, riv);
    in  = _urcrypt_cbc_pad(&length, message);
    out = urcrypt_malloc(length);
    AES_cbc_encrypt(in, out, length, &aes_key, riv, AES_ENCRYPT);
    urcrypt_free(in);

    _urcrypt_reverse_inplace(length, out);
    *out_length = length;
    return out;
  }
}
