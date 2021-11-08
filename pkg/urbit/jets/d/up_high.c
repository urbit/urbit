#include "all.h"

u3_noun
u3qdu_high(u3_atom a)
{
  u3_atom u, v;
  u3_noun pro;

  u = u3qc_xeb(a);
  v = u3qc_bex(u);

  pro = u3qc_rsh(0, 1, v);

  u3z(u);
  u3z(v);

  return pro;
}

u3_noun
u3wdu_high(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_high(a);
  }
}

