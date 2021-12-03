#include "all.h"

u3_noun
u3qdu_qor_see(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun n_a, t_a, m_a;
  u3x_trel(a, &n_a, &t_a, &m_a);

  if (u3_nul == t_a) {
    return u3nc(c3__sing, u3k(n_a));
  }

  u3_atom hot = u3h(t_a);

  if ( c3n == u3ud(hot) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hot ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      u3_noun p_t_a = u3t(u3t(t_a));

      u3_noun n_p_t_a, l_p_t_a, m_p_t_a, r_p_t_a;
      u3x_qual(p_t_a, &n_p_t_a, &l_p_t_a, &m_p_t_a, &r_p_t_a);

      return u3nt(
               c3__play,
               u3nt(u3k(n_p_t_a), u3k(l_p_t_a), u3k(m_p_t_a)),
               u3nt(u3k(n_a), u3k(r_p_t_a), u3k(m_a)));
    }

    case c3__rlos: {
      u3_noun p_t_a = u3t(u3t(t_a));

      u3_noun n_p_t_a, l_p_t_a, m_p_t_a, r_p_t_a;
      u3x_qual(p_t_a, &n_p_t_a, &l_p_t_a, &m_p_t_a, &r_p_t_a);

      return u3nt(
               c3__play,
               u3nt(u3k(n_a), u3k(l_p_t_a), u3k(m_p_t_a)),
               u3nt(u3k(n_p_t_a), u3k(r_p_t_a), u3k(m_a)));
    }
  }
}

u3_noun
u3wdu_qor_see(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_see(a);
  }
}


