#include "all.h"

u3_noun
u3qdu_pert(u3_atom k, u3_atom l)
{
  u3_noun u;
  u3_noun pro;

  u = u3qc_mix(k, l);
  pro = u3qdu_high(u);

  u3z(u);

  return pro;
}

u3_noun
u3wdu_pert(u3_noun cor)
{
  u3_noun a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_pert(a, b);
  }
}

