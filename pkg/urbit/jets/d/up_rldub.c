#include "all.h"

u3_noun
u3qdu_rldub(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  if ( c3n == u3du(r_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_r_a = u3t(u3t(r_a));
  u3_noun hor = u3h(r_a);

  if ( c3n == u3ud(hor) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hor ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      u3_noun n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a;

      u3x_qual(p_r_a, &n_p_r_a, &l_p_r_a, &m_p_r_a, &r_p_r_a);

      u3_noun pre = u3qdu_lrsin(n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a);
      u3_noun pro = u3qdu_rlsin(n_a, l_a, m_a, pre);

      u3z(pre);

      return pro;
    }

    case c3__rlos: {
      u3_noun n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a;

      u3x_qual(p_r_a, &n_p_r_a, &l_p_r_a, &m_p_r_a, &r_p_r_a);

      u3_noun pre = u3qdu_rrsin(n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a);
      u3_noun pro = u3qdu_rlsin(n_a, l_a, m_a, pre);

      u3z(pre);

      return pro;
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
    u3_noun n, l, m, r;
    u3x_qual(a, &n, &l, &m, &r);

    if ( (c3n == u3du(n)) || (c3n == u3ud(m)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qdu_rldub(n, l, m, r);
    }
  }
}

