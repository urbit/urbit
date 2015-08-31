/* j/3/hor.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_hor(u3_noun a,
           u3_noun b)
  {
    if ( c3y == u3ud(a) ) {
      if ( c3y == u3ud(b) ) {
        return u3qc_gor(a, b);
      } else {
        return c3y;
      }
    } else {
      if ( c3y == u3ud(b) ) {
        return c3n;
      }
      else {
        u3_noun h_a = u3h(a);
        u3_noun h_b = u3h(b);

        if ( c3y == u3r_sing(h_a, h_b) ) {
          return u3qc_gor(u3t(a), u3t(b));
        } else {
          return u3qc_gor(h_a, h_b);
        }
      }
    }
  }
  u3_noun
  u3wc_hor(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_hor(a, b);
    }
  }

