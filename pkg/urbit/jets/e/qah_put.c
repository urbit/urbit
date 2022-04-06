#include "all.h"

u3_noun
u3qe_qah_put(u3_noun a, u3_noun k, u3_noun p, u3_noun v)
{
  u3_noun pre = u3qe_qah_del(a, k);
  u3_noun pro = u3qe_qah_raw(pre, k, p, v);

  u3z(pre);

  return pro;
}

u3_noun
u3we_qah_put(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                        u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qah_put(a, k, p, v);
  }
}


