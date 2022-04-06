#include "all.h"

u3_noun
u3qe_qah_rule(u3_atom k, u3_atom p, u3_noun v, u3_noun a, u3_noun b)
{
  u3_atom l = u3h(u3t(a));
  u3_atom m = u3qe_qah_peak(k, l);

  if (c3y == u3qe_qah_zero(m, l)) {
    return u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(a), u3k(b)));
  }
  else
  {
    return u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(b), u3k(a)));
  }
}

u3_noun
u3we_qah_rule(u3_noun cor)
{
  u3_atom k, p;
  u3_noun v, a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &k, u3x_sam_6, &p, u3x_sam_14, &v,
                      u3x_sam_30, &a, u3x_sam_31, &b, 0)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qah_rule(k, p, v, a, b);
  }
}


