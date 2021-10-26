#ifndef KECCAK_FIPS202_H
#define KECCAK_FIPS202_H
#define __STDC_WANT_LIB_EXT1__ 1
#include <stdint.h>
#include <stdlib.h>

#define HASH224_SIZE 28
#define HASH256_SIZE 32
#define HASH384_SIZE 48
#define HASH512_SIZE 64

#define decshake(bits) \
  int shake##bits(uint8_t*, size_t, const uint8_t*, size_t);

#define decsha3(bits) \
  int sha3_##bits(uint8_t*, size_t, const uint8_t*, size_t);

#define deckeccak(bits) \
  int keccak_##bits(uint8_t*, size_t, const uint8_t*, size_t);

decshake(128)
decshake(256)

decsha3(224)
decsha3(256)
decsha3(384)
decsha3(512)

deckeccak(224)
deckeccak(256)
deckeccak(384)
deckeccak(512)

#endif
