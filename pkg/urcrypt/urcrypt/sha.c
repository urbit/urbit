#include "urcrypt.h"
#include "util.h"
#include <openssl/sha.h>

void
urcrypt_sha1(uint8_t *message, size_t length, uint8_t out[20])
{
  urcrypt__reverse(length, message);
  SHA1(message, length, out);
  urcrypt__reverse(20, out);
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

