/* j/6/fine.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqf_fine(
                    u2_noun fuv,
                    u2_noun lup,
                    u2_noun mar)
  {
    if ( (c3__void == lup) || (c3__void == mar) ) {
      return c3__void;
    } else {
      return u2nq(c3__fine, u2k(fuv),
                                    u2k(lup),
                                    u2k(mar));
    }
  }
  u2_noun
  u2_cwf_fine(
                   u2_noun cor)
  {
    u2_noun fuv, lup, mar;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &fuv,
                               u2_cv_sam_6, &lup,
                               u2_cv_sam_7, &mar, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_fine(fuv, lup, mar);
    }
  }
