#include "all.h"

u3_noun
u3qdu_toy(u3_noun a, u3_noun b)
{
  if ( u3_nul == a ) {
    return u3k(b);
  }
  else if ( u3_nul == b ) {
    return u3k(a);
  }
  else {
    u3_noun n_a, t_a, m_a;
    u3_noun n_b, t_b, m_b;
    u3x_trel(a, &n_a, &t_a, &m_a);
    u3x_trel(b, &n_b, &t_b, &m_b);

    u3_noun k_n_a, p_n_a, v_n_a;
    u3_noun k_n_b, p_n_b, v_n_b;
    u3x_trel(n_a, &k_n_a, &p_n_a, &v_n_a);
    u3x_trel(n_b, &k_n_b, &p_n_b, &v_n_b);

    if ( c3y == u3qdu_win(p_n_a, k_n_a, p_n_b, k_n_b) ) {
      return u3nt(u3k(n_a), u3qdu_rbal(n_b, t_a, m_a, t_b), u3k(m_b));
    }
    else {
      return u3nt(u3k(n_b), u3qdu_lbal(n_a, t_a, m_a, t_b), u3k(m_b));
    }
  }
}

u3_noun
u3wdu_toy(u3_noun cor)
{
  u3_noun a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0 )) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_toy(a, b);
  }
}

