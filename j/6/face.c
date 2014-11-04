/* j/6/face.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_face(
                    u3_noun cog,
                    u3_noun tip)
  {
    if ( c3__void == tip ) {
      return c3__void;
    }
    else return u3nt
      (c3__face, u3k(cog), u3k(tip));
  }
  u3_noun
  u3_cwf_face(
                   u3_noun cor)
  {
    u3_noun cog, tip;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &cog, u3_cv_sam_3, &tip, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_face(cog, tip);
    }
  }
