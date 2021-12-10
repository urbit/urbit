#ifndef DEFINEMACROS_H
#define DEFINEMACROS_H

/*** Helper macros to define SHA3 and SHAKE instances. ***/
#define defshake(bits)                                                            \
  int shake##bits(uint8_t* out, size_t outlen, const uint8_t* in, size_t inlen) { \
    return hash(out, outlen, in, inlen, 200 - (bits / 4), 0x1f);                  \
  }
#define defsha3(bits)                                                             \
  int sha3_##bits(uint8_t* out, size_t outlen, const uint8_t* in, size_t inlen) { \
    if (outlen > (bits / 8)) {                                                    \
      return -1;                                                                  \
    }                                                                             \
    return hash(out, outlen, in, inlen, 200 - (bits / 4), 0x06);                  \
  }

#define defkeccak(bits)                                                             \
  int keccak_##bits(uint8_t* out, size_t outlen, const uint8_t* in, size_t inlen) { \
    if (outlen > (bits / 8)) {                                                      \
      return -1;                                                                    \
    }                                                                               \
    return hash(out, outlen, in, inlen, 200 - (bits / 4), 0x01);                    \
  }

/*** FIPS202 SHAKE VOFs ***/
defshake(128) defshake(256)

    /*** FIPS202 SHA3 FOFs ***/
    defsha3(224) defsha3(256) defsha3(384) defsha3(512)

    /*** Non FIP202 SHA3 (KECCAK) FOFs ***/
    defkeccak(224) defkeccak(256) defkeccak(384) defkeccak(512)

#endif  // DEFINEMACROS_H
