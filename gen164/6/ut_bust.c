/* j/6/bust.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, bust)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_noun yam = j2_mcy(Pt6, ut, burn)(van, sut);
    u2_noun yib = u2nc
      (
       u2nc(u2k(u2h(u2h(yam))),
                    u2k(dib)),
       u2k(u2t(yam)));
    u2_noun woo = u2nc(c3__cnzy, u2_blip);
    u2_noun wox = j2_mcy(Pt6, ut, mint)(van, sut, c3__noun, woo);
    u2_noun ret = u2_cn_nock_on(yib, u2k(u2t(wox)));

    if ( u2_none == ret ) {
      return u2_cm_error("bust-nock");
    }
    u2z(wox);
    u2z(woo);
    u2z(yam);

    return ret;
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, bust)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, bust)(u2_noun cor)                               //  retain
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, bust)(van, sut, dib);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, bust)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
#if 1
    return j2_mcx(Pt6, ut, bust)(van, sut, dib);
#else
    c3_m    fun_m = c3__bust;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, dib);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = j2_mcx(Pt6, ut, bust)(van, sut, dib);

      return u2_cz_save_2(fun_m, sut, dib, pro);
    }
#endif
  }

  u2_weak
  j2_mck(Pt6, ut, bust)(u2_noun cor)
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(dib));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, bust)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, bust),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, bust), c3__bust,
    },
    { }
  };
