/* j/6/fine.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_fine(
                    u3_noun fuv,
                    u3_noun lup,
                    u3_noun mar)
  {
    if ( (c3__void == lup) || (c3__void == mar) ) {
      return c3__void;
    } else {
      return u3nq(c3__fine, u3k(fuv),
                                    u3k(lup),
                                    u3k(mar));
    }
  }
  u3_noun
  u3_cwf_fine(
                   u3_noun cor)
  {
    u3_noun fuv, lup, mar;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &fuv,
                               u3_cv_sam_6, &lup,
                               u3_cv_sam_7, &mar, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_fine(fuv, lup, mar);
    }
  }
