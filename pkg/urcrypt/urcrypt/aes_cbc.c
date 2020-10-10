#include "urcrypt.h"
#include "util.h"
#include <string.h>
#include <openssl/aes.h>

static int
urcrypt__cbc_pad(uint8_t **message_ptr,
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

static int
urcrypt__cbc_help(uint8_t **message_ptr,
                  size_t *length_ptr,
                  const AES_KEY *key,
                  uint8_t ivec[16],
                  const int enc,
                  urcrypt_realloc_t realloc_ptr)
{
  if ( 0 != urcrypt__cbc_pad(message_ptr, length_ptr, realloc_ptr) ) {
    return -1;
  }
  else {
    uint8_t *out = *message_ptr;
    size_t length = *length_ptr;
    urcrypt__reverse(16, ivec);
    urcrypt__reverse(length, out);
    AES_cbc_encrypt(out, out, length, key, ivec, enc);
    urcrypt__reverse(length, out);
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

  urcrypt__reverse(16, key);

  if ( 0 != AES_set_encrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
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

  urcrypt__reverse(16, key);

  if ( 0 != AES_set_decrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
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

  urcrypt__reverse(24, key);

  if ( 0 != AES_set_encrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
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

  urcrypt__reverse(24, key);

  if ( 0 != AES_set_decrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
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

  urcrypt__reverse(32, key);

  if ( 0 != AES_set_encrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
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

  urcrypt__reverse(32, key);

  if ( 0 != AES_set_decrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    return urcrypt__cbc_help(message_ptr, length_ptr,
        &aes_key, ivec, AES_DECRYPT, realloc_ptr);
  }
}
