/* j/6/tack.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3_cqfu_tack(u3_noun van,
                        u3_noun sut,
                        u3_noun peh,
                        u3_noun mur)
  {
    u3_noun von = u3_ci_molt(u3k(van), u3_cv_sam, u3k(sut), 0);
    u3_noun gat = u3_cj_hook(u3k(von), "tack");

    return u3_cn_kick_on
        (u3_ci_molt(gat, u3_cv_sam_2, u3k(peh), u3_cv_sam_3, u3k(mur), 0));
  }
