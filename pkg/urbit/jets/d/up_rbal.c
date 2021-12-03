#include "all.h"

u3_noun
u3qdu_rbal(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  u3_atom sl = u3qdu_size(l_a);
  u3_atom sr = u3qdu_size(r_a);
  u3_atom s  = u3qa_add(sl, sr);

  if ( c3y == u3qa_lth(s, 2) ) {
    u3z(sl);
    u3z(sr);
    u3z(s);

    return u3qdu_rlos(n_a, l_a, m_a, r_a);
  }
  else {
    u3z(s);

    u3_atom sm_l = u3qa_mul(4, sl);

    if ( c3y == u3qa_gth(sr, sm_l) ) {
      u3z(sl);
      u3z(sr);
      u3z(sm_l);

      return u3qdu_rlbal(n_a, l_a, m_a, r_a);
    }
    else {
      u3z(sr);
      u3z(sm_l);

      u3_atom sm_r = u3qa_mul(4, sr);

      if ( c3y == u3qa_gth(sl, sm_r) ) {
        u3z(sl);
        u3z(sm_r);

        return u3qdu_rrbal(n_a, l_a, m_a, r_a);
      }
      else {
        u3z(sl);
        u3z(sm_r);

        return u3qdu_rlos(n_a, l_a, m_a, r_a);
      }
    }
  }
}

u3_noun
u3wdu_rbal(u3_noun cor)
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
      return u3qdu_rbal(n, l, m, r);
    }
  }
}

