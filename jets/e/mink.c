/* j/5/mink.c
**
*/
#include "all.h"


  u3_noun
  u3we_mink(u3_noun cor)
  {
    u3_noun bus, fol, fly;

    if ( c3n == u3r_mean(cor, u3x_sam_4, &bus,
                              u3x_sam_5, &fol,
                              u3x_sam_3, &fly,
                              0) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun som;

      som = u3n_nock_in(u3k(fly), u3k(bus), u3k(fol));

      return som;
    }
  }

  u3_noun
  u3we_mick(u3_noun cor)
  {
    u3_noun bus, fol, sea;

    if ( c3n == u3r_mean(cor, u3x_sam_4, &bus,
                              u3x_sam_5, &fol,
                              u3x_sam_3, &sea,
                              0) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun som;

      som = u3n_nock_it(u3k(sea), u3k(bus), u3k(fol));

      return som;
    }
  }
