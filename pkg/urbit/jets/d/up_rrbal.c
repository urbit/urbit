#include "all.h"

u3_noun
u3qdu_rrbal(u3_noun n, u3_noun l, u3_noun m, u3_noun r)
{
  if ( c3n == u3du(l) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_l = u3t(u3t(l));

  u3_noun n_p_l, l_p_l, m_p_l, r_p_l;

  u3x_qual(p_l, &n_p_l, &l_p_l, &m_p_l, &r_p_l);

  u3_atom sl = u3qdu_size(l_p_l);
  u3_atom sr = u3qdu_size(r_p_l);

  c3_o comp = u3qa_gth(sl, sr);

  u3z(sl);
  u3z(sr);

  if ( c3y == comp ) {
    return u3qdu_rrsin(n, l, m, r);
  }
  else {
    return u3qdu_rrdub(n, l, m, r);
  }
}

u3_noun
u3wdu_rrbal(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
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
      return u3qdu_rrbal(n, l, m, r);
    }
  }
}

