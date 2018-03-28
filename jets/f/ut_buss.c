/* j/6/ut_buss.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _cqfu_buss(u3_noun van,
             u3_noun sut,
             u3_noun hot,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "buss");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(hot),
                                u3x_sam_3,
                                u3k(gen),
                                0));
  }

  static u3_noun
  _cqfu_bust(u3_noun van,
             u3_noun sut,
             u3_noun hot,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "bust");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(hot),
                                u3x_sam_3,
                                u3k(gen),
                                0));
  }

  static u3_noun
  _cqfu_busk(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "busk");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam,
                                u3k(gen),
                                0));
  }

/* boilerplate
*/
  u3_noun
  u3wfu_buss(u3_noun cor)
  {
    u3_noun sut, hot, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &hot,
                               u3x_sam_3, &gen,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3du(hot)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_buss(van, sut, hot, gen);
    }
  }

  u3_noun
  u3qfu_buss(u3_noun van,
             u3_noun sut,
             u3_noun hot,
             u3_noun gen)
  {
    return _cqfu_buss(van, sut, hot, gen);
  }

  u3_noun
  u3wfu_bust(u3_noun cor)
  {
    u3_noun sut, hot, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &hot,
                               u3x_sam_3, &gen,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3du(hot)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_bust(van, sut, hot, gen);
    }
  }

  u3_noun
  u3qfu_bust(u3_noun van,
             u3_noun sut,
             u3_noun hot,
             u3_noun gen)
  {
    return _cqfu_bust(van, sut, hot, gen);
  }


  u3_noun
  u3wfu_busk(u3_noun cor)
  {
    u3_noun sut, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &gen,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_busk(van, sut, gen);
    }
  }

  u3_noun
  u3qfu_busk(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    return _cqfu_busk(van, sut, gen);
  }

