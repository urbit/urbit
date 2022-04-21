#include "all.h"

u3_noun
u3qdu_rule(u3_atom k, u3_atom p, u3_noun v, u3_noun a, u3_noun b)
{
  u3_atom l = u3h(u3t(a));
  u3_atom m = u3qdu_peak(k, l);

  return (c3y == u3qdu_zero(m, l))
       ? u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(a), u3k(b)))
       : u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(b), u3k(a)));
}

u3_noun
u3wdu_rule(u3_noun cor)
{
  u3_atom k, p;
  u3_noun v, a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &k, u3x_sam_6, &p, u3x_sam_14, &v,
                      u3x_sam_30, &a, u3x_sam_31, &b, 0)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3du(v)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_rule(k, p, v, a, b);
  }
}



