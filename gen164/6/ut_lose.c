/* j/6/lose.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, lose)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "lose");

    if ( u2_none == hoc ) {
      c3_assert(!"register lose");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(gat, u2_cv_sam, u2k(gen), 0);

      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, lose)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_noun cor = j2_mci(Pt6, ut, lose)(van, sut, gen);

    return u2_cn_kick_on(cor);
  }
