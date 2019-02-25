/* j/6/gain.c
**
*/
#include "all.h"


  u3_noun
  u3qfu_gain(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("u3qfu_gain-gain", von, "gain");

    return u3n_kick_on(u3i_molt(gat, u3x_sam, u3k(gen), 0));
  }
