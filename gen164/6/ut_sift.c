/* j/6/sift.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, sift)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, sift)(
                       u2_noun cor)                               //  retain
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

  u2_noun                                                         //  transfer
  u2_cqfu_sift(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return u2k(ref);
  }

  u2_weak
  j2_mck(Pt6, ut, sift)(
                        u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc
        (u2k(sut), u2k(ref));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, sift)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, sift),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, sift), c3__sift
    },
    { }
  };

