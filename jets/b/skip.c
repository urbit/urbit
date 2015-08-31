/* j/2/skip.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_skip(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3_none;
    } else {
      u3_noun hoz = u3n_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun vyr = u3qb_skip(u3t(a), b);

      switch ( hoz ) {
        case c3y:  return vyr;
        case c3n:  return u3nc(u3k(u3h(a)), vyr);
        default:   u3z(hoz);
                   u3z(vyr);
                   return u3_none;
      }
    }
  }
  u3_noun
  u3wb_skip(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3_none;
    } else {
      return u3qb_skip(a, b);
    }
  }

