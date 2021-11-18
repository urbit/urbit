#include "all.h"

u3_noun
u3qdu_qor_top(u3_noun a)
{
  if (u3_nul == a) {
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun m_a = u3t(u3t(a));
    return u3k(m_a);
  }
}

u3_noun
u3wdu_qor_top(u3_noun cor)
{
  u3_noun a;

  if (
    (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
    (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_top(a);
  }
}

