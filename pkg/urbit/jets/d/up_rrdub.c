#include "all.h"

u3_noun
u3qdu_rrdub(u3_noun a)
{
  u3_noun n_a, l_a, m_a, r_a;

  u3x_qual(a, &n_a, &l_a, &m_a, &r_a);

  if ( c3n == u3du(l_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_l_a;

  u3x_cell(u3t(l_a), 0, &p_l_a);

  u3_noun hol = u3h(l_a);

  if ( c3n == u3ud(hol) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hol ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      return u3qdu_rrsin(
               u3nq(u3k(n_a), u3qdu_llsin(p_l_a), u3k(m_a), u3k(r_a)));
    }

    case c3__rlos: {
      return u3qdu_rrsin(
               u3nq(u3k(n_a), u3qdu_rlsin(p_l_a), u3k(m_a), u3k(r_a)));
    }
  }
}

u3_noun
u3wdu_rrdub(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_rrdub(a);
  }
}

