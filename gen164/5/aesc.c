/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

#if defined(U2_OS_osx)
#include <CommonCrypto/CommonCryptor.h>
#else
#include <openssl/aes.h>
#endif

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pt5, aesc, en)[];
  extern u2_ho_jet j2_mcj(Pt5, aesc, de)[];

/* functions
*/
  u2_weak
  j2_mcd(Pt5, aesc, en)(u2_wire wir_r,
                        u2_atom a,
                        u2_atom b)
  {
    c3_y         a_y[32];
    c3_y         b_y[16];
#if defined(U2_OS_osx)
    size_t       siz_i;
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
                               16, b_y, 16, &siz_i) ||
         16 != siz_i )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
#else
    if ( 0 != AES_set_encrypt_key(a_y, 256, &key_u) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      AES_encrypt(b_y, b_y, &key_u);
    }
#endif

    return u2_ci_bytes(16, b_y);
  }

  u2_weak
  j2_mc(Pt5, aesc, en)(u2_wire wir_r,
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2_stud(a) ||
         u2_no == u2_stud(b) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      return j2_mcd(Pt5, aesc, en)(wir_r, a, b);
    }
  }

  u2_weak
  j2_mcd(Pt5, aesc, de)(u2_wire wir_r,
                        u2_atom a,
                        u2_atom b)
  {
    c3_y    a_y[32];
    c3_y    b_y[16];
#if defined(U2_OS_osx)
    size_t  siz_i;
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
                               16, b_y, 16, &siz_i) ||
         16 != siz_i )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
#else
    if ( 0 != AES_set_decrypt_key(a_y, 256, &key_u) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      AES_decrypt(b_y, b_y, &key_u);
    }
#endif

    return u2_ci_bytes(16, b_y);
  }

  u2_weak
  j2_mc(Pt5, aesc, de)(u2_wire wir_r,
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2_stud(a) ||
         u2_no == u2_stud(b) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      return j2_mcd(Pt5, aesc, de)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt5, aesc, en)[] = {
    { ".2", c3__lite, j2_mc(Pt5, aesc, en), Tier5, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, aesc, de)[] = {
    { ".2", c3__lite, j2_mc(Pt5, aesc, de), Tier5, u2_none, u2_none },
    { }
  };

  u2_ho_driver
  j2_mbd(Pt5, aesc)[] = {
    { j2_sc(Pt5, aesc, en), j2_mcj(Pt5, aesc, en), 0, 0, u2_none },
    { j2_sc(Pt5, aesc, de), j2_mcj(Pt5, aesc, de), 0, 0, u2_none },
    { }
  };

  u2_ho_driver
  j2_db(Pt5, aesc) =
    { j2_sb(Pt5, aesc), 0, j2_mbd(Pt5, aesc), 0, u2_none };
