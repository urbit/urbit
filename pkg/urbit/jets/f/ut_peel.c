/* j/6/ut_peel.c
**
*/
#include "all.h"

  u3_noun
  _cqfu_peel(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun axe)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_peel-peel", von, "peel");

    gat = u3i_molt(gat, u3x_sam_2, u3k(way), u3x_sam_3, u3k(axe), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat))); 
  }

/* boilerplate
*/
  u3_noun
  u3wfu_peel(u3_noun cor)
  {
    u3_noun sut, way, axe, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &axe,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_peel(van, sut, way, axe);
    }
  }

  u3_noun
  u3qfu_peel(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun axe)
  {
    return _cqfu_peel(van, sut, way, axe);
  }

