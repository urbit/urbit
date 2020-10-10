#include "urcrypt.h"
#include "util.h"
#include <aes_siv.h>

static AES_SIV_CTX*
urcrypt__aes_siv_init(uint8_t *key,
                      size_t key_length,
                      urcrypt_aes_siv_data *data,
                      size_t data_length)
{
  AES_SIV_CTX *ctx = AES_SIV_CTX_new();
  if ( NULL == ctx ) {
    return NULL;
  }
  else {
    urcrypt__reverse(key_length, key);
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
        urcrypt__reverse(len, dat);
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
urcrypt__aes_siv_en(uint8_t *key,
                    size_t key_length,
                    uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t iv[16],
                    uint8_t *out)
{
  AES_SIV_CTX *ctx = urcrypt__aes_siv_init(key, key_length, data, data_length);

  if ( NULL == ctx ) {
    return -1;
  }
  else {
    int ret;
    urcrypt__reverse(message_length, message);
    ret = AES_SIV_EncryptFinal(ctx, iv, out, message, message_length);
    AES_SIV_CTX_free(ctx);

    if ( 0 == ret ) {
      return -2;
    }
    else {
      urcrypt__reverse(16, iv);
      urcrypt__reverse(message_length, out);
      return 0;
    }
  }
}

static int
urcrypt__aes_siv_de(uint8_t *key,
                    size_t key_length,
                    uint8_t *message,
                    size_t message_length,
                    urcrypt_aes_siv_data *data,
                    size_t data_length,
                    uint8_t iv[16],
                    uint8_t *out)
{
  AES_SIV_CTX *ctx = urcrypt__aes_siv_init(key, key_length, data, data_length);

  if ( NULL == ctx ) {
    return -1;
  }
  else {
    int ret;

    urcrypt__reverse(message_length, message);
    urcrypt__reverse(16, iv);
    ret = AES_SIV_DecryptFinal(ctx, out, iv, message, message_length);
    AES_SIV_CTX_free(ctx);

    if ( 0 == ret ) {
      return -2;
    }
    else {
      urcrypt__reverse(message_length, out);
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
  return urcrypt__aes_siv_en(key, 32,
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
  return urcrypt__aes_siv_de(key, 32,
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
  return urcrypt__aes_siv_en(key, 48,
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
  return urcrypt__aes_siv_de(key, 48,
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
  return urcrypt__aes_siv_en(key, 64,
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
  return urcrypt__aes_siv_de(key, 64,
      message, message_length, data, data_length, iv, out);
}
