/* j/5/shax.c
**
*/
#include "all.h"


#if defined(U3_OS_osx)
#include <CommonCrypto/CommonDigest.h>
#else
#include <openssl/sha.h>
#endif

/* functions
*/

  u3_noun
  u3qe_shay(u3_atom a,
            u3_atom b)
  {
    c3_assert(_(u3a_is_cat(a)));
    c3_y* fat_y = c3_malloc(a + 1);

    u3r_bytes(0, a, fat_y, b);
    {
      c3_y dig_y[32];
#if defined(U3_OS_osx)
      CC_SHA256_CTX ctx_h;

      CC_SHA256_Init(&ctx_h);
      CC_SHA256_Update(&ctx_h, fat_y, a);
      CC_SHA256_Final(dig_y, &ctx_h);
#else
      SHA256_CTX ctx_h;

      SHA256_Init(&ctx_h);
      SHA256_Update(&ctx_h, fat_y, a);
      SHA256_Final(dig_y, &ctx_h);
#endif
      free(fat_y);
      return u3i_bytes(32, dig_y);
    }
  }

//   u3_noun
//   u3qe_shax(
//                     u3_atom a)
//   {
//     c3_w  met_w = u3r_met(3, a);
//     return u3qe_shay(met_w, a);
//   }
//  XX  preformance
u3_noun
  u3qe_shax(u3_atom a)
  {
    c3_w  met_w = u3r_met(3, a);
    c3_y* fat_y = c3_malloc(met_w + 1);

    u3r_bytes(0, met_w, fat_y, a);
    {
      c3_y dig_y[32];
#if defined(U3_OS_osx)
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
      return u3i_bytes(32, dig_y);
    }
  }

//  XX end preformance

  u3_noun
  u3qe_shal(u3_atom a,
            u3_atom b)
  {
    c3_assert(_(u3a_is_cat(a)));
    c3_y* fat_y = c3_malloc(a + 1);

    u3r_bytes(0, a, fat_y, b);
    {
      c3_y dig_y[64];
#if defined(U3_OS_osx)
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
      return u3i_bytes(64, dig_y);
    }
  }

  u3_noun
  u3qe_shas(u3_atom sal,
            u3_atom ruz)
  {
    u3_noun one = u3qe_shax(ruz);
    u3_noun two = u3qc_mix(sal, one);
    u3_noun tri = u3qe_shax(two);

    u3z(one); u3z(two); return tri;
  }

  u3_noun
  u3we_shax(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_shax(a);
    }
  }

  u3_noun
  u3we_shay(u3_noun cor)
  {
    u3_noun a, b;
    
//     static int few = 0;
//     if(few == 0) printf("foo\r\n");
//     few++; few %= 1000;
      

    if ( (u3_none == (a = u3r_at(u3x_sam_2, cor))) ||
         (u3_none == (b = u3r_at(u3x_sam_3, cor))) ||
         (c3n == u3ud(a)) ||
         (c3n == u3a_is_cat(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_shay(a, b);
    }
  }

  u3_noun
  u3we_shal(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_none == (a = u3r_at(u3x_sam_2, cor))) ||
         (u3_none == (b = u3r_at(u3x_sam_3, cor))) ||
         (c3n == u3ud(a)) ||
         (c3n == u3a_is_cat(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_shal(a, b);
    }
  }

  u3_noun
  u3we_shas(u3_noun cor)
  {
    u3_noun sal, ruz;

    if ( (u3_none == (sal = u3r_at(u3x_sam_2, cor))) ||
         (u3_none == (ruz = u3r_at(u3x_sam_3, cor))) ||
         (c3n == u3ud(sal)) ||
         (c3n == u3ud(ruz)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_shas(sal, ruz);
    }
  }

  static u3_noun
  _og_list(u3_noun a,
           u3_noun b,
           u3_noun c)
  {
    u3_noun l = u3_nul;

    if ( !_(u3a_is_cat(b)) ) {
      return u3m_bail(c3__fail);
    }
    while ( 0 != b ) {
      u3_noun x = u3qc_mix(a, c);
      u3_noun y = u3qc_mix(b, x);
      u3_noun d = u3qe_shas(c3_s4('o','g','-','b'), y);
      u3_noun m;

      u3z(x); u3z(y);

      if ( b < 256 ) {
        u3_noun e = u3qc_end(0, b, d);

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
    return u3kb_flop(l);
  }

  u3_noun
  u3qeo_raw(u3_atom a,
            u3_atom b)
  {
    u3_noun x = u3qc_mix(b, a);
    u3_noun c = u3qe_shas(c3_s4('o','g','-','a'), x);
    u3_noun l = _og_list(a, b, c);
    u3_noun r = u3qc_can(0, l);

    u3z(l);
    u3z(c);
    u3z(x);

    return r;
  }

  u3_noun
  u3weo_raw(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qeo_raw(a, b);
    }
  }
