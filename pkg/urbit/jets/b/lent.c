/* j/2/lent.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_lent(u3_noun a)
  {
    u3_noun len = 0;

    while ( 1 ) {
      if ( 0 == a ) {
        return len;
      }
      else if ( c3n == u3du(a) ) {
        u3z(len);
        return u3m_bail(c3__exit);
      }
      else {
        len = u3i_vint(len);
        a = u3t(a);
      }
    }
  }
  u3_noun
  u3wb_lent(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_lent(a);
    }
  }
  u3_noun
  u3kb_lent(u3_noun a)
  {
    u3_noun b = u3qb_lent(a);

    u3z(a);
    return b;
  }

