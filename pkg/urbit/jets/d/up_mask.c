#include "all.h"

u3_noun
u3qdu_mask(u3_atom a)
{
  u3_atom u, v, w, x, y;
  u3_atom pro;

  u = u3qc_bex(5);
  v = u3qc_bex(u);
  w = u3qa_dec(v);

  // use u3i_word for 0xffffffff

  x = u3qa_dec(a);
  y = u3qc_mix(x, w);

  pro = u3qc_mix(y, a);

  u3z(u);
  u3z(v);
  u3z(w);
  u3z(x);
  u3z(y);

  return pro;
}

u3_noun
u3wdu_mask(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_mask(a);
  }
}

