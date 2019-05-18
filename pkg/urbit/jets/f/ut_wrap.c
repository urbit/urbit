/* j/6/wrap.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_wrap(u3_noun van,
             u3_noun sut,
             u3_noun yoz)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_wrap-wrap", von, "wrap");

    gat = u3i_molt(gat, u3x_sam, u3k(yoz), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

/* boilerplate
*/
  u3_noun
  u3wfu_wrap(u3_noun cor)
  {
    u3_noun sut, yoz, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &yoz, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_wrap(van, sut, yoz);
    }
  }

  u3_noun
  u3qfu_wrap(u3_noun van,
             u3_noun sut,
             u3_noun yoz)
  {
    return _cqfu_wrap(van, sut, yoz);
  }
