/* j/5/shax.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

#if defined(U2_OS_linux) || defined(U2_OS_freebsd)
#include <openssl/sha.h>
#elif defined(U2_OS_osx)
#include <CommonCrypto/CommonDigest.h>
#else
#error "port: sha256"
#endif

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
#if defined(U2_OS_linux) || defined(U2_OS_freebsd)
      SHA256_CTX ctx_h;

      SHA256_Init(&ctx_h);
      SHA256_Update(&ctx_h, fat_y, met_w);
      SHA256_Final(dig_y, &ctx_h);
#elif defined(U2_OS_osx)
      CC_SHA256_CTX ctx_h;

      CC_SHA256_Init(&ctx_h);
      CC_SHA256_Update(&ctx_h, fat_y, met_w);
      CC_SHA256_Final(dig_y, &ctx_h);
#else
      #error "port: sha256"
#endif
      return u2_rl_bytes(wir_r, 32, dig_y);
    }
  }

  u2_weak                                                         //  produce
  j2_mbc(Pt5, shas)(u2_wire wir_r, 
                    u2_atom sal,                                  //  retain
                    u2_atom ruz)                                  //  retain
  {
    u2_noun one = j2_mbc(Pt5, shax)(wir_r, ruz);
    u2_noun two = j2_mbc(Pt3, mix)(wir_r, sal, one);
    u2_noun tri = j2_mbc(Pt5, shax)(wir_r, two);

    u2z(one); u2z(two); return tri;
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, shax)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cv_sam, cor))) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt5, shax)(wir_r, a);
    }
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, shas)(u2_wire wir_r, 
                   u2_noun cor)                                    //  retain
  {
    u2_noun sal, ruz;

    if ( (u2_none == (sal = u2_frag(u2_cv_sam_2, cor))) ||
         (u2_none == (ruz = u2_frag(u2_cv_sam_3, cor))) ||
         (u2_no == u2_stud(sal)) ||
         (u2_no == u2_stud(ruz)) ) 
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt5, shas)(wir_r, sal, ruz);
    }
  }

  static u2_noun                                                  //  produce
  _og_list(u2_wire wir_r,
           u2_noun a,                                             //  retain
           u2_noun b,                                             //  retain
           u2_noun c)                                             //  retain
  {
    u2_noun l = u2_nul;

    if ( !u2_fly_is_cat(b) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    while ( 0 != b ) {
      u2_noun x = j2_mbc(Pt3, mix)(wir_r, a, c);
      u2_noun y = j2_mbc(Pt3, mix)(wir_r, b, x);
      u2_noun d = j2_mbc(Pt5, shas)(wir_r, c3_s4('o','g','-','b'), y);
      u2_noun m;

      u2z(x); u2z(y);

      if ( b < 256 ) {
        u2_noun e = j2_mbc(Pt3, end)(wir_r, 0, b, d);
        
        u2z(d);
        m = u2nc(b, e);
        b = 0;
      } else {
        m = u2nc(256, d);
        c = d;

        b -= 256;
      }
      l = u2nc(m, l);
    }
    return u2_ckb_flop(l);
  }
           
  u2_noun                                                         //  produce
  j2_mcc(Pt5, og, raw)(u2_wire wir_r, 
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    u2_noun x = j2_mbc(Pt3, mix)(wir_r, b, a);
    u2_noun c = j2_mbc(Pt5, shas)(wir_r, c3_s4('o','g','-','a'), x);
    u2_noun l = _og_list(wir_r, a, b, c);
    u2_noun r = j2_mbc(Pt3, can)(wir_r, 0, l);

    u2z(l);
    u2z(c);
    u2z(x);

    return r;
  }

  u2_weak                                                         //  transfer
  j2_mc(Pt5, og, raw)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt5, og, raw)(wir_r, a, b);
    }
  }



/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, shax)[] = { 
    { ".2", c3__lite, j2_mb(Pt5, shax), Tier5, u2_none, u2_none },
    { }
  };
  u2_ho_jet 
  j2_mbj(Pt5, shas)[] = { 
    { ".2", c3__lite, j2_mb(Pt5, shas), Tier5, u2_none, u2_none },
    { }
  };

  u2_ho_jet 
  j2_mcj(Pt5, og, raw)[] = {
    { ".2", c3__lite, j2_mc(Pt5, og, raw), Tier5, u2_none, u2_none },
    { }
  };

  u2_ho_driver 
  j2_mbd(Pt5, og)[] = {
    { j2_sc(Pt5, og, raw), j2_mcj(Pt5, og, raw), 0, 0, u2_none },
    {}
  };

  u2_ho_driver
  j2_db(Pt5, og) = 
    { j2_sb(Pt5, og), 0, j2_mbd(Pt5, og), 0, u2_none };

