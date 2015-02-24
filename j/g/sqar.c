/* j/1/sqgr.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qg_sqar(u3_atom a)
  {
    mpz_t a_mp;

    fprintf(stderr, "C squared %d!\r\n", a);

    u3r_mp(a_mp, a);
    mpz_mul(a_mp, a_mp, a_mp);

    return u3i_mp(a_mp);
  }
  u3_noun
  u3wg_sqar(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qg_sqar(a);
    }
  }
