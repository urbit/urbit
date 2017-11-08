/* j/2/skid.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_skid(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return u3nc(u3_nul, u3_nul);
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    } else {
      u3_noun acc = u3qb_skid(u3t(a), b);
      u3_noun hoz = u3n_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun nex;

      if ( c3y == hoz ) {
        nex = u3nc(u3nc(u3k(u3h(a)), u3k(u3h(acc))), u3k(u3t(acc)));
      } 
      else {
        nex = u3nc(u3k(u3h(acc)), u3nc(u3k(u3h(a)), u3k(u3t(acc))));
      }
      u3z(hoz);
      u3z(acc);

      return nex;
    }
  }
  u3_noun
  u3wb_skid(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_skid(a, b);
    }
  }
