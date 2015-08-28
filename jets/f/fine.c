/* j/6/fine.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_fine(u3_noun fuv,
            u3_noun lup,
            u3_noun mar)
  {
    if ( (c3__void == lup) || (c3__void == mar) ) {
      return c3__void;
    } else {
      return u3nq(c3__fine, 
                  u3k(fuv),
                  u3k(lup),
                  u3k(mar));
    }
  }
  u3_noun
  u3wf_fine(u3_noun cor)
  {
    u3_noun fuv, lup, mar;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &fuv,
                              u3x_sam_6, &lup,
                              u3x_sam_7, &mar, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_fine(fuv, lup, mar);
    }
  }
