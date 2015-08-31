/* j/3/bex.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_bex(u3_atom a)
  {
    mpz_t a_mp;

    if ( !_(u3a_is_cat(a)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      mpz_init_set_ui(a_mp, 1);
      mpz_mul_2exp(a_mp, a_mp, a);

      return u3i_mp(a_mp);
    }
  }
  u3_noun
  u3wc_bex(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_bex(a);
    }
  }

