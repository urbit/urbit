/* j/1/lth.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qa_lth(u3_atom a,
           u3_atom b)
  {
    if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
      return __(a < b);
    }
    else if ( 0 == a ) {
      return c3y;
    }
    else if ( 0 == b ) {
      return c3n;
    }
    else {
      mpz_t   a_mp, b_mp;
      u3_noun cmp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      cmp = (mpz_cmp(a_mp, b_mp) < 0) ? c3y : c3n;

      mpz_clear(a_mp);
      mpz_clear(b_mp);

      return cmp;
    }
  }
  u3_noun
  u3wa_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(b) && a != 0) ||
         (c3n == u3ud(a) && b != 0) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qa_lth(a, b);
    }
  }
