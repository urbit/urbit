/* j/5/mink.c
**
** This file is in the public domain.
*/
#include "all.h"


  u2_noun
  u2_cwe_mink(u2_noun cor)
  {
    u2_noun bus, fol, fly;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_4, &bus,
                               u2_cv_sam_5, &fol,
                               u2_cv_sam_3, &fly,
                               0) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return u2_cn_nock_in(u2k(bus), u2k(fol), u2k(fly));
    }
  }
