#include "all.h"

u3_noun
u3qdu_rrbal(u3_noun a)
{
  u3_noun n_a, l_a, m_a, r_a;

  u3x_qual(a, &n_a, &l_a, &m_a, &r_a);

  if ( c3n == u3du(l_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_l_a;
  u3_noun l_p_l_a, r_p_l_a;

  u3x_cell(u3t(r_a), 0, &p_l_a);
  u3x_qual(p_l_a, 0, &l_p_l_a, 0, &r_p_l_a);

  if ( (c3n == u3du(l_p_l_a)) || (c3n == u3du(r_p_l_a)) ) {
    return u3m_bail(c3__exit);
  }

  u3_atom sl = u3qdu_size(l_p_l_a);
  u3_atom sr = u3qdu_size(r_p_l_a);

  if ( c3y == u3qa_gth(sl, sr) ) {
    return u3qdu_rrsin(a);
  }
  else {
    return u3qdu_rrdub(a);
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
    return u3qdu_rrbal(a);
  }
}

