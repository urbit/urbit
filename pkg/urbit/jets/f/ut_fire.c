/* j/6/fire.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_fire(u3_noun van,
             u3_noun sut,
             u3_noun hag)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_fire-fire", von, "fire");

    gat = u3i_molt(gat, u3x_sam, u3k(hag), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));

  }

/* boilerplate
*/
  u3_noun
  u3wfu_fire(u3_noun cor)
  {
    u3_noun sut, hag, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &hag, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_fire(van, sut, hag);
    }
  }

  u3_noun
  u3qfu_fire(u3_noun van,
                        u3_noun sut,
                        u3_noun hag)
  {
    return _cqfu_fire(van, sut, hag);
  }
