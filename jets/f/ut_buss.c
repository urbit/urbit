/* j/6/ut_buss.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_buss(u3_noun van,
             u3_noun sut,
             u3_noun cog,
             u3_noun gen)
  {
    u3_noun cug, ret;

    cug = u3nc(u3nt(u3nt(u3k(cog), u3_nul, u3k(gen)), 
                    u3_nul, 
                    u3_nul),
               u3_nul);
    ret = u3qf_face(cug, sut);

    u3z(cug);
    return ret;
  }

/* boilerplate
*/
  u3_noun
  u3wfu_buss(u3_noun cor)
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
      return _cqfu_buss(van, sut, cog, gen);
    }
  }

  u3_noun
  u3qfu_buss(u3_noun van,
             u3_noun sut,
             u3_noun cog,
             u3_noun gen)
  {
    return _cqfu_buss(van, sut, cog, gen);
  }
