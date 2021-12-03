#include "all.h"

u3_noun
u3qdu_gone(u3_atom k, u3_atom l, u3_atom m)
{
  u3_noun n = u3qdu_mask(m);
  u3_noun a = u3qc_dis(k, n);
  u3_noun b = u3qc_dis(l, n);

  u3_noun pro = ( a == b ) ? c3n : c3y;

  u3z(n);
  u3z(a);
  u3z(b);

  return pro;
}

u3_noun
u3wdu_gone(u3_noun cor)
{
  u3_noun a, b, c;

  if (
    (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &b, u3x_sam_7, &c, 0 )) ||
    (c3n == u3ud(a)) ||
    (c3n == u3ud(b)) ||
    (c3n == u3ud(c)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_gone(a, b, c);
  }
}

