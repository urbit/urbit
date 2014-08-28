/* j/6/ut_conk.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_bean                                                         //  transfer
  j2_mcx(Pt6, ut, conk)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun got)                              //  retain
  {
    if ( u2_yes == u2ud(got) ) {
      return j2_mby(Pt6, face)(got, sut);
    }
    else switch ( u2h(got) ) {
      default: return u2_cm_bail(c3__fail);

      case 0: {
        return u2k(sut);
      }
      case 1: {
        return j2_mby(Pt6, face)
          (u2h(u2t(got)),
                 j2_mcx(Pt6, ut, conk)(van, sut, u2t(u2t(got))));
      }
      case 2: {
        u2_bean vet = u2_cr_at(j2_ut_van_vet, van);
        u2_noun hed, tal, ret;

        if ( u2_yes == vet ) {
          u2_noun cel = u2nt(c3__cell, c3__noun, c3__noun);

          if ( u2_no == j2_mcy(Pt6, ut, nest)(van, cel, u2_yes, sut) ) {
            return u2_cm_bail(c3__fail);
          }
          u2z(cel);
        }
        hed = j2_mcy(Pt6, ut, peek)(van, sut, c3__both, 2);
        tal = j2_mcy(Pt6, ut, peek)(van, sut, c3__both, 3);

        ret = j2_mby(Pt6, cell)
          (
           j2_mcx(Pt6, ut, conk)(van, hed, u2h(u2t(got))),
           j2_mcx(Pt6, ut, conk)(van, tal, u2t(u2t(got))));

        u2z(hed);
        u2z(tal);

        return ret;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, conk)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, conk)(u2_noun cor)                               //  retain
  {
    u2_noun sut, got, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &got,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, conk)(van, sut, got);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, conk)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun got)                              //  retain
  {
    return j2_mcx(Pt6, ut, conk)(van, sut, got);
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, conk)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, conk), Tier6_b, u2_none, u2_none },
    { }
  };

