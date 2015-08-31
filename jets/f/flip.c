/* j/6/flip.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_flip(u3_noun hel)
  {
    if ( c3y == u3r_sing(1, u3h(hel)) ) {
      if ( (c3y == u3t(hel)) ) {
        return u3nc(1, c3n);
      }
      else {
        c3_assert((c3n == u3t(hel)));

        return u3nc(1, c3y);
      }
    }
    else {
      return u3nq(6,
                  u3k(hel),
                  u3nc(1, c3n),
                  u3nc(1, c3y));
    }
  }
  u3_noun
  u3wf_flip(u3_noun cor)
  {
    u3_noun hel;

    if ( u3_none == (hel = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_flip(hel);
    }
  }
