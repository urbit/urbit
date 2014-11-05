/* j/1/gth.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqa_gth(u3_atom a,
                   u3_atom b)
  {
    if ( _(u3_ca_is_cat(a)) && _(u3_ca_is_cat(b)) ) {
      return __(a > b);
    }
    else {
      mpz_t   a_mp, b_mp;
      u3_bean cmp;

      u3_cr_mp(a_mp, a);
      u3_cr_mp(b_mp, b);

      cmp = (mpz_cmp(a_mp, b_mp) > 0) ? c3y : c3n;

      mpz_clear(a_mp);
      mpz_clear(b_mp);

      return cmp;
    }
  }
  u3_noun
  u3_cwa_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqa_gth(a, b);
    }
  }

  u3_noun
  u3_cka_gth(u3_noun a, u3_noun b)
  {
    u3_noun c = u3_cqa_gth(a, b);

    u3z(a); u3z(b);
    return c;
  }
