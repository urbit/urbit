#include "all.h"

u3_noun
u3qdu_llbal(u3_noun n, u3_noun l, u3_noun m, u3_noun r)
{
  if ( c3n == u3du(r) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_r = u3t(u3t(r));

  u3_noun n_p_r, l_p_r, m_p_r, r_p_r;
  u3x_qual(p_r, &n_p_r, &l_p_r, &m_p_r, &r_p_r);

  u3_atom sl = u3qdu_size(l_p_r);
  u3_atom sr = u3qdu_size(r_p_r);

  c3_o comp = u3qa_lth(sl, sr);

  u3z(sl);
  u3z(sr);

  return ( c3y == comp ) ? u3qdu_llsin(n, l, m, r) : u3qdu_lldub(n, l, m, r);
}

u3_noun
u3wdu_llbal(u3_noun cor)
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
      return u3qdu_llbal(n, l, m, r);
    }
  }
}

