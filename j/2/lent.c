/* j/2/lent.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_lent(u3_noun a)
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
  u3_cwb_lent(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3r_at(u3v_sam, cor)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3_cqb_lent(a);
    }
  }
  u3_noun
  u3_ckb_lent(u3_noun a)
  {
    u3_noun b = u3_cqb_lent(a);

    u3z(a);
    return b;
  }

