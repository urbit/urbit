/* j/2/murn.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_murn(u3_noun a, u3_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun one = u3n_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun two = u3qb_murn(u3t(a), b);
      u3_noun nex;

      switch ( u3ud(one) ) {
        case c3y:  u3z(one);
                   return two;
        case c3n:  nex = u3nc(u3k(u3t(one)), two);
                   u3z(one);
                   return nex;
        default:   u3z(one);
                   u3z(two);
                   return u3_none;
      }
    }
  }
  u3_noun
  u3wb_murn(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_murn(a, b);
    }
  }

