/* j/6/lose.c
**
** This file is in the public domain.
*/
#include "all.h"


  u2_noun
  u2_cqfu_lose(u2_noun van,
                        u2_noun sut,
                        u2_noun gen)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
    u2_noun gat = u2_cj_hook(u2k(von), "lose");

    return u2_cn_kick_on(u2_ci_molt(gat, u2_cv_sam, u2k(gen), 0));
  }
