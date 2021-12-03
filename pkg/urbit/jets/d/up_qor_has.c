#include "all.h"

c3_o
u3qdu_qor_has(u3_noun a, u3_noun k)
{
  u3_noun pre = u3qdu_qor_get(a, k);

  if (u3_nul == pre) {
    return c3n;
  }
  else {
    u3z(pre);

    return c3y;
  }
}

u3_noun
u3wdu_qor_has(u3_noun cor)
{
  u3_noun a, k;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_has(a, k);
  }
}



