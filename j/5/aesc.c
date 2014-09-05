/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"


#if defined(U2_OS_osx)
#include <CommonCrypto/CommonCryptor.h>
#else
#include <openssl/aes.h>
#endif

/* functions
*/
  u2_noun
  u2_cqea_en(u2_atom a, u2_atom b)
  {
    c3_y         a_y[32];
    c3_y         b_y[16];
#if defined(U2_OS_osx)
    size_t       siz_i = 0;
#else
    AES_KEY key_u;
#endif

    c3_assert(u2_cr_met(3, a) <= 32);
    c3_assert(u2_cr_met(3, b) <= 16);

    u2_cr_bytes(0, 32, a_y, a);
    u2_cr_bytes(0, 16, b_y, b);

#if defined(U2_OS_osx)
    if ( kCCSuccess != CCCrypt(kCCEncrypt, kCCAlgorithmAES128,
                               kCCOptionECBMode, a_y, kCCKeySizeAES256, 0, b_y,
                               16, b_y, 16, &siz_i) )
    {
      return u2_cm_bail(c3__exit);
    }
    else c3_assert(16 == siz_i);
#else
    if ( 0 != AES_set_encrypt_key(a_y, 256, &key_u) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      AES_encrypt(b_y, b_y, &key_u);
    }
#endif

    return u2_ci_bytes(16, b_y);
  }

  u2_noun
  u2_cwea_en(u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return u2_cqea_en(a, b);
    }
  }

  u2_noun
  u2_cqea_de(u2_atom a, u2_atom b)
  {
    c3_y    a_y[32];
    c3_y    b_y[16];
#if defined(U2_OS_osx)
    size_t  siz_i = 0;
#else
    AES_KEY key_u;
#endif

    c3_assert(u2_cr_met(3, a) <= 32);
    c3_assert(u2_cr_met(3, b) <= 16);

    u2_cr_bytes(0, 32, a_y, a);
    u2_cr_bytes(0, 16, b_y, b);

#if defined(U2_OS_osx)
    if ( kCCSuccess != CCCrypt(kCCDecrypt, kCCAlgorithmAES128,
                               kCCOptionECBMode, a_y, kCCKeySizeAES256, 0, b_y,
                               16, b_y, 16, &siz_i) )
    {
      return u2_cm_bail(c3__exit);
    }
    else c3_assert(16 == siz_i);
#else
    if ( 0 != AES_set_decrypt_key(a_y, 256, &key_u) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      AES_decrypt(b_y, b_y, &key_u);
    }
#endif

    return u2_ci_bytes(16, b_y);
  }

  u2_noun
  u2_cwea_de(u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return u2_cqea_de(a, b);
    }
  }
