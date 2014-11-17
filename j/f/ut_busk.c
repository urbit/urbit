/* j/6/ut_busk.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_busk(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun cog,
                        u3_noun hyp)
  {
    u3_noun sep = u3qfu_seep(van, sut, c3__both, hyp);
    u3_noun bid = u3nt(u3k(cog), u3k(hyp), sep);
    u3_noun ret = u3qf_bull(bid, sut);

    u3z(bid);

    return ret;
  }

/* boilerplate
*/
  u3_noun
  u3wfu_busk(u3_noun cor)
  {
    u3_noun sut, cog, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &cog,
                                u3x_sam_3, &hyp,
                                u3x_con, &van,
                                0)) ||
         (c3n == u3ud(cog)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_busk(van, sut, cog, hyp);
    }
  }

  u3_noun
  u3qfu_busk(u3_noun van,
                        u3_noun sut,
                        u3_noun cog,
                        u3_noun hyp)
  {
    return _cqfu_busk(van, sut, cog, hyp);
  }
