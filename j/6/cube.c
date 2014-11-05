/* j/6/cube.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_cube(
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
  u3_cwf_cube(
                   u3_noun cor)
  {
    u3_noun dil, goq;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &dil, u3_cv_sam_3, &goq, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_cube(dil, goq);
    }
  }
