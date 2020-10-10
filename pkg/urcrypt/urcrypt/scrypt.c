#include "urcrypt.h"
#include <libscrypt.h>
#include <sha256.h>

void
urcrypt_scrypt_pbk(const uint8_t *passwd,
                   size_t passwdlen,
                   const uint8_t *salt,
                   size_t saltlen,
                   uint64_t count,
                   size_t outlen, // must be at most 32*(2^32-1)
                   uint8_t *out)
{
  libscrypt_PBKDF2_SHA256(
      passwd, passwdlen, salt, saltlen, count, out, outlen);
}
