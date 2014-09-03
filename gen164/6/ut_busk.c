/* j/6/ut_busk.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  _cqfu_busk(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun cog,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_noun sep = u2_cqfu_seep(van, sut, c3__both, hyp);
    u2_noun bid = u2nt(u2k(cog), u2k(hyp), sep);
    u2_noun ret = u2_cqf_bull(bid, sut);

    u2z(bid);

    return ret;
  }

/* boilerplate
*/
  u2_noun                                                         //  transfer
  u2_cwfu_busk(u2_noun cor)                               //  retain
  {
    u2_noun sut, cog, hyp, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &cog,
                                u2_cv_sam_3, &hyp,
                                u2_cv_con, &van,
                                0)) ||
         (u2_no == u2ud(cog)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_busk(van, sut, cog, hyp);
    }
  }

  u2_noun                                                         //  transfer
  u2_cqfu_busk(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun cog,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    return _cqfu_busk(van, sut, cog, hyp);
  }

  u2_weak
  j2_mck(Pt6, ut, busk)(u2_noun cor)
  {
    u2_noun sut, hyp, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_3, &hyp, u2_cv_con, &van, &hyp, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(hyp));
    }
  }
