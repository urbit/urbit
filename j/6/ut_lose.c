/* j/6/lose.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3_cqfu_lose(u3_noun van,
                        u3_noun sut,
                        u3_noun gen)
  {
    u3_noun von = u3_ci_molt(u3k(van), u3_cv_sam, u3k(sut), 0);
    u3_noun gat = u3_cj_hook(u3k(von), "lose");

    return u3_cn_kick_on(u3_ci_molt(gat, u3_cv_sam, u3k(gen), 0));
  }
