/* j/6/tack.c
**
** This file is in the public domain.
*/
#include "all.h"


  u2_noun
  u2_cqfu_tack(u2_noun van,
                        u2_noun sut,
                        u2_noun peh,
                        u2_noun mur)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
    u2_noun gat = u2_cj_hook(u2k(von), "tack");

    return u2_cn_kick_on
        (u2_ci_molt(gat, u2_cv_sam_2, u2k(peh), u2_cv_sam_3, u2k(mur), 0));
  }
