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
  u3_noun
  u3_cqe_shax(
                    u3_atom a)
  {
    c3_w  met_w = u3_cr_met(3, a);
    c3_y* fat_y = c3_malloc(met_w + 1);

    u3_cr_bytes(0, met_w, fat_y, a);
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
      return u3_ci_bytes(32, dig_y);
    }
  }

  u3_noun
  u3_cqe_shal(u3_atom a,
                    u3_atom b)
  {
    c3_assert(u3_so(u3_co_is_cat(a)));
    c3_y* fat_y = c3_malloc(a + 1);

    u3_cr_bytes(0, a, fat_y, b);
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
      return u3_ci_bytes(64, dig_y);
    }
  }

  u3_noun
  u3_cqe_shas(
                    u3_atom sal,
                    u3_atom ruz)
  {
    u3_noun one = u3_cqe_shax(ruz);
    u3_noun two = u3_cqc_mix(sal, one);
    u3_noun tri = u3_cqe_shax(two);

    u3z(one); u3z(two); return tri;
  }

  u3_noun
  u3_cwe_shax(
                  u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam, cor))) ||
         (u3_no == u3ud(a)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqe_shax(a);
    }
  }

  u3_noun
  u3_cwe_shal(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam_2, cor))) ||
         (u3_none == (b = u3_cr_at(u3_cv_sam_3, cor))) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3_co_is_cat(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqe_shal(a, b);
    }
  }

  u3_noun
  u3_cwe_shas(u3_noun cor)
  {
    u3_noun sal, ruz;

    if ( (u3_none == (sal = u3_cr_at(u3_cv_sam_2, cor))) ||
         (u3_none == (ruz = u3_cr_at(u3_cv_sam_3, cor))) ||
         (u3_no == u3ud(sal)) ||
         (u3_no == u3ud(ruz)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqe_shas(sal, ruz);
    }
  }

  static u3_noun
  _og_list(
           u3_noun a,
           u3_noun b,
           u3_noun c)
  {
    u3_noun l = u3_nul;

    if ( u3_ne(u3_co_is_cat(b)) ) {
      return u3_cm_bail(c3__fail);
    }
    while ( 0 != b ) {
      u3_noun x = u3_cqc_mix(a, c);
      u3_noun y = u3_cqc_mix(b, x);
      u3_noun d = u3_cqe_shas(c3_s4('o','g','-','b'), y);
      u3_noun m;

      u3z(x); u3z(y);

      if ( b < 256 ) {
        u3_noun e = u3_cqc_end(0, b, d);

        u3z(d);
        m = u3nc(b, e);
        b = 0;
      } else {
        m = u3nc(256, d);
        c = d;

        b -= 256;
      }
      l = u3nc(m, l);
    }
    return u3_ckb_flop(l);
  }

  u3_noun
  u3_cqeo_raw(u3_atom a, u3_atom b)
  {
    u3_noun x = u3_cqc_mix(b, a);
    u3_noun c = u3_cqe_shas(c3_s4('o','g','-','a'), x);
    u3_noun l = _og_list(a, b, c);
    u3_noun r = u3_cqc_can(0, l);

    u3z(l);
    u3z(c);
    u3z(x);

    return r;
  }

  u3_noun
  u3_cweo_raw(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqeo_raw(a, b);
    }
  }
