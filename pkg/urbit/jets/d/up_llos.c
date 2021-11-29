#include "all.h"

u3_noun
u3qdu_llos(u3_noun n, u3_noun l, u3_atom m, u3_noun r)
{
  u3_noun pro;

  u3_atom sl = u3qdu_size(l);
  u3_atom sr = u3qdu_size(r);
  u3_atom s  = u3qa_add(sl, sr);

  pro = u3qa_inc(s);

  u3z(sl);
  u3z(sr);
  u3z(s);

  return u3nt(c3__llos, pro, u3nq(n, l, m, r));
}

u3_noun
u3wdu_llos(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    u3_noun n, l, m, r;

    u3x_qual(a, &n, &l, &m, &r);

    if ( (c3n == u3du(n)) || (c3n == u3ud(m)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qdu_llos(u3k(n), u3k(l), u3k(m), u3k(r));
    }
  }
}
