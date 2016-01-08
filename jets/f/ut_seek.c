/* j/6/seek.c
**
*/
#include "all.h"


  u3_noun
  u3qfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "seek");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(way), 
                                u3x_sam_3, 
                                u3k(hyp),
                                0));
  }

  u3_noun
  u3qfu_sick(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "sick");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(way), 
                                u3x_sam_3, 
                                u3k(hyp),
                                0));
  }
