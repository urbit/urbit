/* j/6/core.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqf_core(
                    u2_noun pac,
                    u2_noun con)
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2nt(c3__core, u2k(pac), u2k(con));
    }
  }
  u2_noun
  u2_cwf_core(
                   u2_noun cor)
  {
    u2_noun pac, con;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &pac, u2_cv_sam_3, &con, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_core(pac, con);
    }
  }
