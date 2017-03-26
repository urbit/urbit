/* j/6/ut_buss.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqzu_buss(u3_noun van,
             u3_noun sut,
             u3_noun cog,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "buss");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(cog), 
                                u3x_sam_3, 
                                u3k(gen),
                                0));
  }

  u3_noun
  _cqzu_busk(u3_noun van,
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
  u3wzu_buss(u3_noun cor)
  {
    u3_noun sut, cog, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &cog,
                               u3x_sam_3, &gen,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3ud(cog)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqzu_buss(van, sut, cog, gen);
    }
  }

  u3_noun
  u3qzu_buss(u3_noun van,
             u3_noun sut,
             u3_noun cog,
             u3_noun gen)
  {
    return _cqzu_buss(van, sut, cog, gen);
  }


  u3_noun
  u3wzu_busk(u3_noun cor)
  {
    u3_noun sut, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &gen,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqzu_busk(van, sut, gen);
    }
  }

  u3_noun
  u3qzu_busk(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    return _cqzu_busk(van, sut, gen);
  }

