/* j/5/mink.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3_cwe_mink(u3_noun cor)
  {
    u3_noun bus, fol, fly;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_4, &bus,
                               u3_cv_sam_5, &fol,
                               u3_cv_sam_3, &fly,
                               0) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cn_nock_in(u3k(fly), u3k(bus), u3k(fol));
    }
  }
