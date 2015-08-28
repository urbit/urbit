/* j/2/bind.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_bind(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return 0;
    } else {
      return u3nc(0, u3n_slam_on(u3k(b), u3k(u3t(a))));
    }
  }
  u3_noun
  u3wb_bind(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_bind(a, b);
    }
  }

