#include "all.h"

u3_noun
u3qdu_zero(u3_atom a, u3_atom b)
{
  u3_atom d = u3qc_dis(a, b);
  u3_atom pro = ( d == 0 ) ? c3y : c3n;

  u3z(d);
  return pro;
}

u3_noun
u3wdu_zero(u3_noun cor)
{
  u3_noun a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_zero(a, b);
  }
}

