/* j/6/toss.c
**
*/
#include "all.h"

  u3_noun
  _cqfu_toss(u3_noun van,
             u3_noun sut,
             u3_noun peh,
             u3_noun mur,
             u3_noun men)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_toss-toss", von, "toss");

    gat = u3i_molt(gat, u3x_sam_2, u3k(peh),
                        u3x_sam_6, u3k(mur),
                        u3x_sam_7, u3k(men),
                        0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

/* boilerplate
*/
  u3_noun
  u3wfu_toss(u3_noun cor)
  {
    u3_noun van, sut, peh, mur, men;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &peh,
                               u3x_sam_6, &mur,
                               u3x_sam_7, &men,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_toss(van, sut, peh, mur, men);
    }
  }

  u3_noun
  u3qfu_toss(u3_noun van,
             u3_noun sut,
             u3_noun peh,
             u3_noun mur,
             u3_noun men)
  {
    return _cqfu_toss(van, sut, peh, mur, men);
  }
