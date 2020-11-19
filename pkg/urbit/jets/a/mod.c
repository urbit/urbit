/* j/1/mod.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qa_mod(u3_atom a,
           u3_atom b)
  {
    if ( 0 == b ) {
      return u3m_bail(c3__exit);
    } else if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
      return a % b;
    } else {
      mpz_t a_mp, b_mp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      mpz_tdiv_r(a_mp, a_mp, b_mp);
      mpz_clear(b_mp);

      return u3i_mp(a_mp);
    }
  }

  u3_noun
  u3wa_mod(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qa_mod(a, b);
    }
  }

u3_noun
u3ka_mod(u3_noun a,
         u3_noun b)
{
  u3_noun c = u3qa_mod(a, b);
  u3z(a); u3z(b);
  return c;
}
