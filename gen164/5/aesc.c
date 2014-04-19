/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

#include <openssl/aes.h>

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
    AES_KEY key_u;
    c3_y    buf_y[32];

    c3_assert(u2_cr_met(3, a) <= 32);
    c3_assert(u2_cr_met(3, b) <= 16);

    u2_cr_bytes(0, 32, buf_y, a);
    if ( 0 != AES_set_encrypt_key(buf_y, 256, &key_u) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      u2_cr_bytes(0, 16, buf_y, b);
      AES_encrypt(buf_y, buf_y, &key_u);
    }

    return u2_ci_bytes(16, buf_y);
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
    AES_KEY key_u;
    c3_y    buf_y[32];

    c3_assert(u2_cr_met(3, a) <= 32);
    c3_assert(u2_cr_met(3, b) <= 16);

    u2_cr_bytes(0, 32, buf_y, a);
    if ( 0 != AES_set_decrypt_key(buf_y, 256, &key_u) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      u2_cr_bytes(0, 16, buf_y, b);
      AES_decrypt(buf_y, buf_y, &key_u);
    }

    return u2_ci_bytes(16, buf_y);
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
