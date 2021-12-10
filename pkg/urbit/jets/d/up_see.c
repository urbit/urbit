#include "all.h"

u3_noun
u3qdu_see(u3_noun a, u3_noun k, u3_noun p)
{
  return u3qdu_qat_see(a, k, p);
}

u3_noun
u3wdu_see(u3_noun cor)
{
  u3_noun a, k, p;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k,
                          u3x_sam_7, &p, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_see(a, k, p);
  }
}


