/* j/6/swab.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/

/* functions
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, snub)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    if ( u2_no == u2du(har) ) {
      return u2_nul;
    } else {
      u2_noun i_har = u2h(har);
      u2_noun t_har = u2t(har);
      u2_noun pi_har = u2h(i_har);
      u2_noun qi_har = u2t(i_har);
      u2_noun peh = j2_mbc(Pt2, flop)(pi_har);
      u2_noun ret = u2nc(u2nc(peh, u2k(qi_har)),
                                 j2_mcx(Pt6, ut, snub)
                                    (van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, snub)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, snub)(
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, har;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &har,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, snub)(van, sut, har);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, snub)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    return j2_mcx(Pt6, ut, snub)(van, sut, har);
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, snub)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, snub), Tier6_c, u2_none, u2_none },
    { }
  };
