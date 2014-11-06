/* j/2/turn.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_turn(u3_noun a, u3_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun one = u3n_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun two = u3qb_turn(u3t(a), b);

      return u3nc(one, two);
    }
  }
  u3_noun
  u3wb_turn(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_turn(a, b);
    }
  }

