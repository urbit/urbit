/* j/6/sift.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* boilerplate
*/
  u2_noun
  u2_cwfu_sift(
                       u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqfu_sift(van, sut, ref);
    }
  }

  u2_noun
  u2_cqfu_sift(u2_noun van,
                        u2_noun sut,
                        u2_noun ref)
  {
    return u2k(ref);
  }
