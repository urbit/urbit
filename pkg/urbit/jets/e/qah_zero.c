#include "all.h"

u3_noun
u3qe_qah_zero(u3_atom m, u3_atom k)
{
  u3_atom h = u3r_mug(k);
  u3_atom d = u3qc_dis(m, h);
  u3_atom pro = ( d == 0 ) ? c3y : c3n;

  u3z(h);
  u3z(d);

  return pro;
}

u3_noun
u3we_qah_zero(u3_noun cor)
{
  u3_noun a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qah_zero(a, b);
  }
}

