/* j/6/sift.c
**
*/
#include "all.h"


/* boilerplate
*/
  u3_noun
  u3wfu_sift(
                       u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &ref,
                                u3x_con, &van,
                                0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_sift(van, sut, ref);
    }
  }

  u3_noun
  u3qfu_sift(u3_noun van,
                        u3_noun sut,
                        u3_noun ref)
  {
    return u3k(ref);
  }
