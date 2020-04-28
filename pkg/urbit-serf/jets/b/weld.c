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
    u3_noun c = u3qb_flop(a);
    u3_noun d = c;

    u3k(b);

    while ( u3_nul != c ) {
      b = u3nc(u3k(u3h(c)), b);
      c = u3t(c);
    }

    u3z(d);

    return b;
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

