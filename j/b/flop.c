/* j/2/flop.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_flop(u3_noun a)
  {
    u3_noun b = 0;

    while ( 1 ) {
      if ( u3_nul == a ) {
        return b;
      }
      else if ( c3n == u3du(a) ) {
        u3z(b);

        return u3m_bail(c3__exit);
      }
      else {
        b = u3nc(u3k(u3h(a)), b);
        a = u3t(a);
      }
    }
  }
  u3_noun
  u3wb_flop(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_flop(a);
    }
  }
  u3_noun
  u3kb_flop(u3_noun a)
  {
    u3_noun b = u3qb_flop(a);

    u3z(a);
    return b;
  }

