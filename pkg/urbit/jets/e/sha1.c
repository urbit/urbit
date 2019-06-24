/* j/5/sha1.c
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
  u3qe_sha1(u3_atom wid, u3_atom dat)
  {
    c3_assert(_(u3a_is_cat(wid)));
    dat = u3qc_rev(3, wid, dat);

    c3_y* fat_y = u3a_malloc(wid + 1);
    u3r_bytes(0, wid, fat_y, dat);
    {
      c3_y dig_y[32];
#if defined(U3_OS_osx)
      CC_SHA1_CTX ctx_h;

      CC_SHA1_Init(&ctx_h);
      CC_SHA1_Update(&ctx_h, fat_y, wid);
      CC_SHA1_Final(dig_y, &ctx_h);
#else
      SHA_CTX ctx_h;

      SHA1_Init(&ctx_h);
      SHA1_Update(&ctx_h, fat_y, wid);
      SHA1_Final(dig_y, &ctx_h);
#endif
      u3a_free(fat_y);
      u3z(dat);
      return u3kc_rev(3, 20, u3i_bytes(20, dig_y));
    }
  }

  u3_noun
  u3we_sha1(u3_noun cor)
  {
    u3_noun wid, dat;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &wid, u3x_sam_3, &dat, 0)) ||
         (c3n == u3ud(wid)) ||
         (c3n == u3a_is_cat(wid)) ||
         (c3n == u3ud(dat)) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qe_sha1(wid, dat);
    }
  }
