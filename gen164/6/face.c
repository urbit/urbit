/* j/6/face.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  u2_cqf_face(
                    u2_noun cog,                                  //  retain
                    u2_noun tip)                                  //  retain
  {
    if ( c3__void == tip ) {
      return c3__void;
    }
    else return u2nt
      (c3__face, u2k(cog), u2k(tip));
  }
  u2_noun                                                         //  transfer
  u2_cwf_face(
                   u2_noun cor)                                   //  retain
  {
    u2_noun cog, tip;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &cog, u2_cv_sam_3, &tip, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_face(cog, tip);
    }
  }
