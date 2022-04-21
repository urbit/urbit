#include "all.h"

u3_noun
u3qdu_qor_rrsin(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  if ( c3n == u3du(l_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun b = u3t(u3t(l_a));

  u3_noun n_b, l_b, m_b, r_b, t_b;
  u3_noun k_n_b;
  u3x_qual(b, &n_b, &l_b, &m_b, &r_b);
  u3x_cell(n_b, &k_n_b, &t_b);

  u3_noun p_n_b = u3h(t_b);
  u3_noun hol = u3h(l_a);

  if ( c3n == u3ud(hol) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hol ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      u3_noun pre = u3qdu_qor_rlos(n_a, r_b, m_a, r_a);
      u3_noun pro = u3qdu_qor_llos(n_b, l_b, m_b, pre);

      u3z(pre);

      return pro;
    }

    case c3__rlos: {
      u3_noun k_n_a;
      u3_noun t_a;

      u3x_cell(n_a, &k_n_a, &t_a);

      u3_noun p_n_a = u3h(t_a);

      if ( (c3n == u3ud(p_n_a)) || (c3n == u3ud(p_n_b)) ) {
        return u3m_bail(c3__exit);
      }

      if ( c3y == u3qdu_lex(p_n_a, k_n_a, p_n_b, k_n_b) ) {
        u3_noun pre = u3qdu_qor_llos(n_b, r_b, m_a, r_a);
        u3_noun pro = u3qdu_qor_rlos(n_a, l_b, m_b, pre);

        u3z(pre);

        return pro;
      }
      else {
        u3_noun pre = u3qdu_qor_rlos(n_a, r_b, m_a, r_a);
        u3_noun pro = u3qdu_qor_rlos(n_b, l_b, m_b, pre);

        u3z(pre);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qor_rrsin(u3_noun cor)
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
      return u3qdu_qor_rrsin(n, l, m, r);
    }
  }
}

