/* j/5/mink.c
**
** This file is in the public domain.
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
      return u3n_nock_in(u3k(fly), u3k(bus), u3k(fol));
    }
  }
