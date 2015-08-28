/* j/3/pow.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_pow(u3_atom a,
           u3_atom b)
  {
    if ( !_(u3a_is_cat(b)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      mpz_t a_mp;

      u3r_mp(a_mp, a);
      mpz_pow_ui(a_mp, a_mp, b);

      return u3i_mp(a_mp);
    }
  }
  u3_noun
  u3wc_pow(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_pow(a, b);
    }
  }

