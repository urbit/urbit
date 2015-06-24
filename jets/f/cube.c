/* j/6/cube.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_cube(
                    u3_noun dil,
                    u3_noun goq)
  {
    if ( c3__void == goq ) {
      return c3__void;
    }
    else return u3nt
      (c3__cube, u3k(dil), u3k(goq));
  }
  u3_noun
  u3wf_cube(
                   u3_noun cor)
  {
    u3_noun dil, goq;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &dil, u3x_sam_3, &goq, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_cube(dil, goq);
    }
  }
