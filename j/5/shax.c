/* j/5/shax.c
**
** This file is in the public domain.
*/
#include "all.h"


#if defined(U2_OS_osx)
#include <CommonCrypto/CommonDigest.h>
#else
#include <openssl/sha.h>
#endif

/* functions
*/
  u2_noun
  u2_cqe_shax(
                    u2_atom a)
  {
    c3_w  met_w = u2_cr_met(3, a);
    c3_y* fat_y = c3_malloc(met_w + 1);

    u2_cr_bytes(0, met_w, fat_y, a);
    {
      c3_y dig_y[32];
#if defined(U2_OS_osx)
      CC_SHA256_CTX ctx_h;

      CC_SHA256_Init(&ctx_h);
      CC_SHA256_Update(&ctx_h, fat_y, met_w);
      CC_SHA256_Final(dig_y, &ctx_h);
#else
      SHA256_CTX ctx_h;

      SHA256_Init(&ctx_h);
      SHA256_Update(&ctx_h, fat_y, met_w);
      SHA256_Final(dig_y, &ctx_h);
#endif
      free(fat_y);
      return u2_ci_bytes(32, dig_y);
    }
  }

  u2_noun
  u2_cqe_shal(u2_atom a,
                    u2_atom b)
  {
    c3_assert(u2_so(u2_co_is_cat(a)));
    c3_y* fat_y = c3_malloc(a + 1);

    u2_cr_bytes(0, a, fat_y, b);
    {
      c3_y dig_y[64];
#if defined(U2_OS_osx)
      CC_SHA512_CTX ctx_h;

      CC_SHA512_Init(&ctx_h);
      CC_SHA512_Update(&ctx_h, fat_y, a);
      CC_SHA512_Final(dig_y, &ctx_h);
#else
      SHA512_CTX ctx_h;

      SHA512_Init(&ctx_h);
      SHA512_Update(&ctx_h, fat_y, a);
      SHA512_Final(dig_y, &ctx_h);
#endif
      free(fat_y);
      return u2_ci_bytes(64, dig_y);
    }
  }

  u2_noun
  u2_cqe_shas(
                    u2_atom sal,
                    u2_atom ruz)
  {
    u2_noun one = u2_cqe_shax(ruz);
    u2_noun two = u2_cqc_mix(sal, one);
    u2_noun tri = u2_cqe_shax(two);

    u2z(one); u2z(two); return tri;
  }

  u2_noun
  u2_cwe_shax(
                  u2_noun cor)
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqe_shax(a);
    }
  }

  u2_noun
  u2_cwe_shal(u2_noun cor)
  {
    u2_noun a, b;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam_2, cor))) ||
         (u2_none == (b = u2_cr_at(u2_cv_sam_3, cor))) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2_co_is_cat(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqe_shal(a, b);
    }
  }

  u2_noun
  u2_cwe_shas(u2_noun cor)
  {
    u2_noun sal, ruz;

    if ( (u2_none == (sal = u2_cr_at(u2_cv_sam_2, cor))) ||
         (u2_none == (ruz = u2_cr_at(u2_cv_sam_3, cor))) ||
         (u2_no == u2ud(sal)) ||
         (u2_no == u2ud(ruz)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqe_shas(sal, ruz);
    }
  }

  static u2_noun
  _og_list(
           u2_noun a,
           u2_noun b,
           u2_noun c)
  {
    u2_noun l = u2_nul;

    if ( u2_ne(u2_co_is_cat(b)) ) {
      return u2_cm_bail(c3__fail);
    }
    while ( 0 != b ) {
      u2_noun x = u2_cqc_mix(a, c);
      u2_noun y = u2_cqc_mix(b, x);
      u2_noun d = u2_cqe_shas(c3_s4('o','g','-','b'), y);
      u2_noun m;

      u2z(x); u2z(y);

      if ( b < 256 ) {
        u2_noun e = u2_cqc_end(0, b, d);

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

  u2_noun
  u2_cqeo_raw(u2_atom a, u2_atom b)
  {
    u2_noun x = u2_cqc_mix(b, a);
    u2_noun c = u2_cqe_shas(c3_s4('o','g','-','a'), x);
    u2_noun l = _og_list(a, b, c);
    u2_noun r = u2_cqc_can(0, l);

    u2z(l);
    u2z(c);
    u2z(x);

    return r;
  }

  u2_noun
  u2_cweo_raw(u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqeo_raw(a, b);
    }
  }
