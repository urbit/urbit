/* j/6/core.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_core(
                    u3_noun pac,
                    u3_noun con)
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u3nt(c3__core, u3k(pac), u3k(con));
    }
  }
  u3_noun
  u3_cwf_core(
                   u3_noun cor)
  {
    u3_noun pac, con;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &pac, u3_cv_sam_3, &con, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_core(pac, con);
    }
  }
