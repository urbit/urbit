/* j/2/levy.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_levy(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return c3y;
    } else {
      u3_noun loz;

      if ( c3n == u3du(a) ) {
        return u3m_bail(c3__exit);
      }
      else switch ( (loz = u3n_slam_on(u3k(b), u3k(u3h(a)))) ) {
        case c3y:  return u3qb_levy(u3t(a), b);
        case c3n:   return c3n;
        default:      u3z(loz);
                      return u3m_bail(c3__exit);
      }
    }
  }
  u3_noun
  u3wb_levy(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_levy(a, b);
    }
  }
