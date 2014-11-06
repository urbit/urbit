/* j/1/gte.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqa_gte(u3_atom a, u3_atom b)
  {
    if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
      return __(a >= b);
    }
    else {
      mpz_t   a_mp, b_mp;
      u3_bean cmp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      cmp = (mpz_cmp(a_mp, b_mp) >= 0) ? c3y : c3n;

      mpz_clear(a_mp);
      mpz_clear(b_mp);

      return cmp;
    }
  }
  u3_noun
  u3_cwa_gte(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3_cqa_gte(a, b);
    }
  }
