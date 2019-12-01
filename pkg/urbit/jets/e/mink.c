/* j/5/mink.c
**
*/
#include "all.h"

  u3_noun
  u3we_mink(u3_noun cor)
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

      c3_assert( c3y == u3du(som) );

      if ( 1 == u3h(som) ) {
        // legacy support: the interpreter now leaves the 0 off the end
        // to match +tono. convert to +tone.
        u3_noun mos = u3nt(1, u3k(u3t(som)), 0);
        u3z(som);
        return mos;
      }
      else {
        return som;
      }
    }
  }

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
