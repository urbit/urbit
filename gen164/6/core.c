/* j/6/core.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, core)(
                    u2_noun pac,                                  //  retain
                    u2_noun con)                                  //  retain
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2nt(c3__core, u2k(pac), u2k(con));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, core)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun pac, con;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &pac, u2_cv_sam_3, &con, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, core)(pac, con);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, core)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, core), Tier6_a, u2_none, u2_none },
    { }
  };
