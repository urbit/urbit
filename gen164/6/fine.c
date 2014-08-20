/* j/6/fine.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, fine)(
                    u2_noun fuv,
                    u2_noun lup,                                  //  retain
                    u2_noun mar)                                  //  retain
  {
    if ( (c3__void == lup) || (c3__void == mar) ) {
      return c3__void;
    } else {
      return u2nq(c3__fine, u2k(fuv),
                                    u2k(lup),
                                    u2k(mar));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, fine)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun fuv, lup, mar;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &fuv,
                               u2_cv_sam_6, &lup,
                               u2_cv_sam_7, &mar, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, fine)(fuv, lup, mar);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, fine)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, fine), Tier6_a, u2_none, u2_none },
    { }
  };
