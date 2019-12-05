/* j/5/mink.c
**
*/
#include "all.h"

  u3_noun
  u3we_mino(u3_noun cor)
  {
    u3_noun bus, fol, gul;

    if ( c3n == u3r_mean(cor, u3x_sam_4, &bus,
                              u3x_sam_5, &fol,
                              u3x_sam_3, &gul,
                              0) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun som;

      som = u3n_nock_et(u3k(gul), u3k(bus), u3k(fol));

      return som;
    }
  }
