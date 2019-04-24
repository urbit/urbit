/* j/2/drop.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_drop(u3_noun a)
  {
    if ( 0 == a ) {
      return u3_nul;
    }
    else {
      return u3nc(0, u3k(u3t(a)));
    }
  }
  u3_noun
  u3wb_drop(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3r_at(u3x_sam, cor)) ) {
      return u3_none;
    } else {
      return u3qb_drop(a);
    }
  }

