/* j/2/reel.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_reel(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(u3r_at(u3x_sam_3, b));
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun gim = u3k(u3h(a));
      u3_noun hur = u3qb_reel(u3t(a), b);

      return u3n_slam_on(u3k(b), u3nc(gim, hur));
    }
  }
  u3_noun
  u3wb_reel(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_reel(a, b);
    }
  }
