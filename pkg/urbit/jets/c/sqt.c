/* j/3/sqt.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_sqt(u3_atom a)
  {
    mpz_t a_mp, b_mp;

    u3r_mp(a_mp, a);
    mpz_init(b_mp);
    mpz_sqrtrem(a_mp, b_mp, a_mp);

    return u3nc(u3k(u3i_mp(a_mp)), u3k(u3i_mp(b_mp)));
  }
  u3_noun
  u3wc_sqt(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_sqt(a);
    }
  }

