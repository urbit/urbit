/* j/6/lose.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3qfu_lose(u3_noun van,
                        u3_noun sut,
                        u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3v_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "lose");

    return u3n_kick_on(u3i_molt(gat, u3v_sam, u3k(gen), 0));
  }
