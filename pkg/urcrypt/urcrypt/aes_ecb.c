#include "urcrypt.h"
#include "util.h"
#include <openssl/aes.h>

int
urcrypt_aes_ecba_en(uint8_t key[16], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(16, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecba_de(uint8_t key[16], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(16, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 128, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_en(uint8_t key[24], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(24, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbb_de(uint8_t key[24], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(24, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 192, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_en(uint8_t key[32], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(32, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_encrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_ENCRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}

int
urcrypt_aes_ecbc_de(uint8_t key[32], uint8_t block[16], uint8_t out[16])
{
  AES_KEY aes_key;

  urcrypt__reverse(32, key);
  urcrypt__reverse(16, block);

  if ( 0 != AES_set_decrypt_key(key, 256, &aes_key) ) {
    return -1;
  }
  else {
    AES_ecb_encrypt(block, out, &aes_key, AES_DECRYPT);
    urcrypt__reverse(16, out);
    return 0;
  }
}
