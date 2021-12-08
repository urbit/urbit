#include "all.h"

u3_noun
u3qdu_qor_bot(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }
  else
  {
    u3_noun n_a, t_a, m_a;
    u3x_trel(a, &n_a, &t_a, &m_a);

    return u3nt(u3_nul, u3k(n_a), u3qdu_sec(t_a, m_a));
  }
}

u3_noun
u3wdu_qor_bot(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_bot(a);
  }
}

