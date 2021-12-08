#include "all.h"

u3_noun
u3qdu_qat_tie(u3_atom k, u3_atom p, u3_noun v, u3_atom l, u3_noun a, u3_noun b)
{
  u3_atom m = u3qdu_pert(k, l);

  if (c3y == u3qdu_zero(m, l)) {
    return u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(a), u3k(b)));
  }
  else
  {
    return u3nq(c3__bin, u3k(k), u3k(p), u3nq(u3k(v), m, u3k(b), u3k(a)));
  }
}

u3_noun
u3wdu_qat_tie(u3_noun cor)
{
  u3_atom k, p, l;
  u3_noun v, a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &k, u3x_sam_6, &p, u3x_sam_14, &v,
                      u3x_sam_30, &l, u3x_sam_62, &a, u3x_sam_63, &b, 0)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3ud(l)) ||
       (c3n == u3du(v)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_tie(k, p, v, l, a, b);
  }
}


