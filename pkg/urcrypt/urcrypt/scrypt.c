#include "urcrypt.h"
#include <libscrypt.h>
#include <sha256.h>

void
urcrypt_scrypt_pbkdf_sha256(const uint8_t *passwd,
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

int
urcrypt_scrypt(const uint8_t *passwd,
               size_t passwdlen,
               const uint8_t *salt,
               size_t saltlen,
               uint64_t n,
               uint32_t r,
               uint32_t p,
               size_t outlen,
               uint8_t *out)
{
  return libscrypt_scrypt(
      passwd, passwdlen, salt, saltlen, n, r, p, out, outlen);
}
