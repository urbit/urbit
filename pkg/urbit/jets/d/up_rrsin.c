#include "all.h"

u3_noun
u3qdu_rrsin(u3_noun a)
{
  u3_noun b;
  u3_noun n_a, l_a, m_a, r_a;
  u3_noun n_b, l_b, m_b, r_b;

  u3_atom p_n_a, k_n_a;
  u3_atom p_n_b, k_n_b;

  u3x_qual(a, &n_a, &l_a, &m_a, &r_a);
  u3x_trel(n_a, &k_n_a, &p_n_a, 0);

  if ( c3n == u3du(l_a) ) {
    return u3m_bail(c3__exit);
  }

  u3x_cell(u3t(l_a), 0, &b);

  u3x_qual(b, &n_b, &l_b, &m_b, &r_b);
  u3x_trel(n_b, &k_n_b, &p_n_b, 0);

  u3_noun hol = u3h(l_a);

  if ( c3n == u3ud(hol) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hol ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      return u3qdu_llos(
               u3k(n_b),
               u3k(l_b),
               u3k(m_b),
               u3qdu_rlos(u3k(n_a), u3k(r_b), u3k(m_a), u3k(r_a)));
    }

    case c3__rlos: {
      if ( c3y == u3qdu_win(p_n_a, k_n_a, p_n_b, k_n_b) ) {
        return u3qdu_rlos(
                 u3k(n_a),
                 u3k(l_b),
                 u3k(m_b),
                 u3qdu_llos(u3k(n_b), u3k(r_b), u3k(m_a), u3k(r_a)));
      }
      else {
        return u3qdu_rlos(
                 u3k(n_b),
                 u3k(l_b),
                 u3k(m_b),
                 u3qdu_rlos(u3k(n_a), u3k(r_b), u3k(m_a), u3k(r_a)));
      }
    }

  }
}

u3_noun
u3wdu_rrsin(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_rrsin(a);
  }
}

