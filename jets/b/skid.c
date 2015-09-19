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

      switch ( hoz ) {
        case c3y:  acc = u3nc(u3nc(u3k(u3h(a)), u3h(acc)), u3t(acc));
                   break;
        case c3n:  acc = u3nc(u3h(acc), u3nc(u3k(u3h(a)), u3t(acc)));
                   break;
        default:   u3z(hoz);
                   return u3m_bail(c3__exit);
      }
      u3z(hoz);
      return acc;
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
