#include "all.h"

u3_noun
_mask(u3_atom a)
{
  u3_atom x = u3qa_dec(a);
  u3_atom y = u3qc_mix(x, 0x7fffffff);

  u3_atom pro = u3qc_mix(y, a);

  u3z(x);
  u3z(y);

  return pro;
}

c3_o
u3qdu_feud(u3_atom m, u3_atom k, u3_atom l)
{
  u3_atom n = _mask(m);
  u3_atom h = u3r_mug(k);
  u3_atom i = u3r_mug(l);
  u3_atom a = u3qc_dis(h, n);
  u3_atom b = u3qc_dis(i, n);

  u3_atom pro = ( a == b ) ? c3n : c3y;

  u3z(n);
  u3z(h);
  u3z(i);
  u3z(a);
  u3z(b);

  return pro;
}

u3_noun
u3wdu_feud(u3_noun cor)
{
  u3_noun m, k, l;

  if (
    (c3n == u3r_mean(cor, u3x_sam_2, &m, u3x_sam_6, &k, u3x_sam_7, &l, 0 )) ||
    (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_feud(m, k, l);
  }
}

