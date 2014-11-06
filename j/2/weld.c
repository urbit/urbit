/* j/2/weld.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_weld(u3_noun a,
                    u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(b);
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3nc(u3k(u3h(a)), u3_cqb_weld(u3t(a), b));
    }
  }
  u3_noun
  u3_cwb_weld(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3_cqb_weld(a, b);
    }
  }
  u3_noun
  u3_ckb_weld(u3_noun a, u3_noun b)
  {
    u3_noun c = u3_cqb_weld(a, b);

    u3z(a); u3z(b);
    return c;
  }

