/* j/5/aes.c
**
*/
#include "all.h"


#if defined(U3_OS_osx)
#include <CommonCrypto/CommonCryptor.h>
#else
#include <openssl/aes.h>
#endif

/* functions
*/
  u3_noun
  u3qea_en(u3_atom a, 
           u3_atom b)
  {
    c3_y         a_y[32];
    c3_y         b_y[16];
#if defined(U3_OS_osx)
    size_t       siz_i = 0;
#else
    AES_KEY key_u;
#endif

    c3_assert(u3r_met(3, a) <= 32);
    c3_assert(u3r_met(3, b) <= 16);

    u3r_bytes(0, 32, a_y, a);
    u3r_bytes(0, 16, b_y, b);

#if defined(U3_OS_osx)
    if ( kCCSuccess != CCCrypt(kCCEncrypt, kCCAlgorithmAES128,
                               kCCOptionECBMode, a_y, kCCKeySizeAES256, 0, b_y,
                               16, b_y, 16, &siz_i) )
    {
      return u3m_bail(c3__exit);
    }
    else c3_assert(16 == siz_i);
#else
    if ( 0 != AES_set_encrypt_key(a_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_encrypt(b_y, b_y, &key_u);
    }
#endif

    return u3i_bytes(16, b_y);
  }

  u3_noun
  u3wea_en(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qea_en(a, b);
    }
  }

  u3_noun
  u3qea_de(u3_atom a,
           u3_atom b)
  {
    c3_y    a_y[32];
    c3_y    b_y[16];
#if defined(U3_OS_osx)
    size_t  siz_i = 0;
#else
    AES_KEY key_u;
#endif

    c3_assert(u3r_met(3, a) <= 32);
    c3_assert(u3r_met(3, b) <= 16);

    u3r_bytes(0, 32, a_y, a);
    u3r_bytes(0, 16, b_y, b);

#if defined(U3_OS_osx)
    if ( kCCSuccess != CCCrypt(kCCDecrypt, kCCAlgorithmAES128,
                               kCCOptionECBMode, a_y, kCCKeySizeAES256, 0, b_y,
                               16, b_y, 16, &siz_i) )
    {
      return u3m_bail(c3__exit);
    }
    else c3_assert(16 == siz_i);
#else
    if ( 0 != AES_set_decrypt_key(a_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_decrypt(b_y, b_y, &key_u);
    }
#endif

    return u3i_bytes(16, b_y);
  }

  u3_noun
  u3wea_de(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qea_de(a, b);
    }
  }
