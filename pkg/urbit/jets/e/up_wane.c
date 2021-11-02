#include "all.h"

u3_noun
u3qe_wane(u3_atom k, u3_atom p, u3_noun v, u3_atom m, u3_noun l, u3_noun r)
{
  if ( u3_nul == r ) {
    if  ( u3_nul == l) {
      return u3nq(c3__tip, u3k(k), u3k(p), u3k(v));
    }
    else {
      return u3nq(c3__bin, u3k(k), u3k(p),
               u3nq(u3k(v), u3k(m), u3k(l), u3_nul));
    }
  }
  else {
    return u3nq(c3__bin, u3k(k), u3k(p),
             u3nq(u3k(v), u3k(m), u3k(l), u3k(r)));
  }
}

u3_noun
u3we_wane(u3_noun cor)
{
  u3_noun k, p, v, m, l, r;

  if (
    (c3n == u3r_mean(cor, u3x_sam_2, &k, u3x_sam_6, &p, u3x_sam_14, &v,
                     u3x_sam_30, &m, u3x_sam_62, &l, u3x_sam_63, &r, 0 )) ||
    (c3n == u3ud(m)) ||
    (c3n == u3ud(p)) ||
    (c3n == u3ud(m))
    )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_wane(k, p, v, m, l, r);
  }
}

