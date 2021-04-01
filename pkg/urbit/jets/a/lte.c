/* j/1/lte.c
**
*/
#include "all.h"

u3_noun
u3qa_lte(u3_atom a, u3_atom b)
{
  if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
    return __(a <= b);
  }
  else if ( 0 == a ) {
    return c3y;
  }
  else if ( 0 == b ) {
    return c3n;
  }
  else {
    c3_w a_w = u3r_met(0, a);
    c3_w b_w = u3r_met(0, b);

    if ( a_w != b_w ) {
      return __(a_w <= b_w);
    }
    else {
      mpz_t   a_mp, b_mp;
      u3_noun cmp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      cmp = (mpz_cmp(a_mp, b_mp) <= 0) ? c3y : c3n;

      mpz_clear(a_mp);
      mpz_clear(b_mp);

      return cmp;
    }
  }
}

u3_noun
u3wa_lte(u3_noun cor)
{
  u3_noun a, b;

  if (  (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0))
     || (c3n == u3ud(b) && 0 != a)
     || (c3n == u3ud(a) && 0 != b) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return u3qa_lte(a, b);
  }
}

u3_noun
u3ka_lte(u3_noun a, u3_noun b)
{
  u3_noun c = u3qa_lte(a, b);
  u3z(a); u3z(b);
  return c;
}
