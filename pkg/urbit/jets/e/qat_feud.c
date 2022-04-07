#include "all.h"

u3_noun
u3qe_mask(u3_atom a)
{

  // XX check implementation.  does this fit into a direct atom?

  // XX as optimisation, use u3i_word to encode 0xffffffff
  u3_atom u = u3qc_bex(5);
  u3_atom v = u3qc_bex(u);
  u3_atom w = u3qa_dec(v);
  u3_atom x = u3qa_dec(a);
  u3_atom y = u3qc_mix(x, w);

  u3_atom pro = u3qc_mix(y, a);

  u3z(u);
  u3z(v);
  u3z(w);
  u3z(x);
  u3z(y);

  return pro;
}

u3_noun
u3qe_qat_feud(u3_atom m, u3_atom k, u3_atom l)
{
  u3_atom n = u3qe_mask(m);
  u3_atom p = u3qc_dis(n, 0x7fffffff);
  u3_atom a = u3qc_dis(k, p);
  u3_atom b = u3qc_dis(l, p);

  u3_atom pro = ( a == b ) ? c3n : c3y;

  u3z(n);
  u3z(p);
  u3z(a);
  u3z(b);

  return pro;
}

u3_noun
u3we_qat_feud(u3_noun cor)
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
    return u3qe_qat_feud(a, b, c);
  }
}

