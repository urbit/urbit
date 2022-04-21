#include "all.h"

c3_o
u3qdu_zero(u3_atom m, u3_noun k)
{
  u3_atom h = u3r_mug(k);
  u3_atom d = u3qc_dis(m, h);
  u3_atom pro = ( d == 0 ) ? c3y : c3n;

  u3z(h);
  u3z(d);

  return pro;
}

u3_noun
u3wdu_zero(u3_noun cor)
{
  u3_noun m, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &m, u3x_sam_3, &k, 0)) ||
       (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_zero(m, k);
  }
}

