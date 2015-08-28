/* j/3/xeb.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_xeb(u3_atom a)
  {
    mpz_t a_mp;

    if ( __(u3a_is_dog(a)) ) {
      u3r_mp(a_mp, a);
      c3_d x = mpz_sizeinbase(a_mp, 2);
      mpz_t b_mp;
      mpz_init_set_ui(b_mp, x);
      return u3i_mp(b_mp);
    }
    else {
      mpz_init_set_ui(a_mp, a);
      c3_d x = mpz_sizeinbase(a_mp, 2);
      mpz_t b_mp;
      mpz_init_set_ui(b_mp, x);
      return u3i_mp(b_mp);
    }
  }
  u3_noun
  u3wc_xeb(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_xeb(a);
    }
  }

