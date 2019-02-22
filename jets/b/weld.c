/* j/2/weld.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_weld(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(b);
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3nc(u3k(u3h(a)), u3qb_weld(u3t(a), b));
    }
  }
  u3_noun
  u3wb_weld(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_weld(a, b);
    }
  }
  u3_noun
  u3kb_weld(u3_noun a,
            u3_noun b)
  {
    u3_noun c = u3qb_weld(a, b);

    u3z(a); u3z(b);
    return c;
  }

