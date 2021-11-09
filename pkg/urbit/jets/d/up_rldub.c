#include "all.h"

u3_noun
u3qdu_rldub(u3_noun a)
{
  u3_noun n_a, l_a, m_a, r_a;

  u3x_qual(a, &n_a, &l_a, &m_a, &r_a);

  if ( c3n == u3du(r_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_r_a;

  u3x_cell(u3t(r_a), 0, &p_r_a);

  u3_noun hor = u3h(r_a);

  if ( c3n == u3ud(hor) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hor ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      return u3qdu_rlsin(
               u3nq(u3k(n_a), u3k(l_a), u3k(m_a),
                    u3qdu_lrsin(p_r_a)));
    }

    case c3__rlos: {
      return u3qdu_llsin(
               u3nq(u3k(n_a), u3k(l_a), u3k(m_a),
                    u3qdu_rrsin(p_r_a)));
    }
  }
}

u3_noun
u3wdu_rldub(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_rldub(a);
  }
}

