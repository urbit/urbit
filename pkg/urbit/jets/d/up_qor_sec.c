#include "all.h"

u3_noun
u3qdu_qor_sec(u3_noun t, u3_atom m)
{
  if ( u3_nul == t ) {
    return u3_nul;
  }
  else {
    u3_noun hol = u3h(t);

    if ( c3n == u3ud(hol) ) {
      return u3m_bail(c3__exit);
    }
    else switch ( hol ) {
      default:
        return u3m_bail(c3__exit);

      case c3__llos: {
        u3_noun n_p_t, l_p_t, m_p_t, r_p_t;
        u3x_qual(u3t(u3t(t)), &n_p_t, &l_p_t, &m_p_t, &r_p_t);

        u3_noun fel = u3nt(u3k(n_p_t), u3k(l_p_t), u3k(m_p_t));

        u3_noun pre = u3qdu_qor_sec(r_p_t, m);
        u3_noun pro = u3qdu_qor_toy(fel, pre);

        u3z(fel);
        u3z(pre);

        return pro;
      }

      case c3__rlos: {
        u3_noun n_p_t, l_p_t, m_p_t, r_p_t;
        u3x_qual(u3t(u3t(t)), &n_p_t, &l_p_t, &m_p_t, &r_p_t);

        u3_noun fer = u3nt(u3k(n_p_t), u3k(r_p_t), u3k(m));

        u3_noun pre = u3qdu_qor_sec(l_p_t, m_p_t);
        u3_noun pro = u3qdu_qor_toy(pre, fer);

        u3z(fer);
        u3z(pre);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qor_sec(u3_noun cor)
{
  u3_noun t, m;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &t, u3x_sam_3, &m, 0 )) ||
       (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_sec(t, m);
  }
}


