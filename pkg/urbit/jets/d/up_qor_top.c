#include "all.h"

u3_noun
u3qdu_qor_top(u3_noun a)
{
  if (u3_nul == a) {
    return u3m_bail(c3__exit);
  }
  else {
    return u3k(u3t(u3t(a)));
  }
}

u3_noun
u3wdu_qor_top(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_top(a);
  }
}

