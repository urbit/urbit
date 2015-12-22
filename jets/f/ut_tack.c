/* j/6/tack.c
**
*/
#include "all.h"

  u3_noun
  u3qfu_tack(u3_noun van,
             u3_noun sut,
             u3_noun peh,
             u3_noun mur)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "tack");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(peh), 
                                u3x_sam_3, 
                                u3k(mur),
                                0));
  }

