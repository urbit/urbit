/* j/6/gain.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_noun                                                         //  transfer
  u2_cqfu_gain(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
    u2_noun gat = u2_cj_hook(u2k(von), "gain");

    return u2_cn_kick_on(u2_ci_molt(gat, u2_cv_sam, u2k(gen), 0));
  }
