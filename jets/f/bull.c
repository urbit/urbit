/* j/6/bull.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_bull(u3_noun bid,
            u3_noun der)
  {
    if ( c3__void == der )
    {
      return c3__void;
    }
    else return u3nt
      (c3__bull, u3k(bid), u3k(der));
  }
  u3_noun
  u3wf_bull(u3_noun cor)
  {
    u3_noun bid, der;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &bid, u3x_sam_3, &der, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_bull(bid, der);
    }
  }
