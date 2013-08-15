/* j/3/shax.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

//  XX OSX-specific
//
#include <CommonCrypto/CommonDigest.h>

/* functions
*/
  u2_weak                                                         //  produce
  j2_mbc(Pt5, shax)(u2_wire wir_r, 
                    u2_atom a)                                    //  retain
  {
    c3_w  met_w = u2_met(3, a);
    c3_y* fat_y = malloc(met_w + 1);

    u2_bytes(0, met_w, fat_y, a);
    {
      c3_y dig_y[32];
      CC_SHA256_CTX ctx_h;

      CC_SHA256_Init(&ctx_h);
      CC_SHA256_Update(&ctx_h, fat_y, met_w);
      CC_SHA256_Final(dig_y, &ctx_h);

      return u2_rl_bytes(wir_r, 32, dig_y);
    }
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, shax)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cw_sam, cor))) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_none;
    } else {
      return j2_mbc(Pt5, shax)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, shax)[] = { 
    { ".3", c3__lite, j2_mb(Pt5, shax), Tier5, u2_none, u2_none },
    { }
  };
