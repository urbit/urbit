#include "all.h"

u3_noun
u3qdu_qor_sink(u3_noun a, u3_noun k, u3_atom p, u3_noun v)
{
  u3_noun b   = u3qdu_qor_put(a, k, p, v);
  u3_noun pre = u3qdu_qor_bot(b);

  u3z(b);

  if (u3_nul == pre) {
    return u3m_bail(c3__exit);
  } else
  {
    u3_noun pro = u3k(u3t(pre));
    u3z(pre);

    return pro;
  }
}

u3_noun
u3wdu_qor_sink(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                      u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_sink(a, k, p, v);
  }
}

