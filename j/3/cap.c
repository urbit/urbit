/* j/3/cap.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_cap(
                   u3_atom a)
  {
    c3_w met_w = u3_cr_met(0, a);

    if ( met_w < 2 ) {
      return u3_cm_bail(c3__exit);
    }
    else if ( (1 == u3_cr_bit((met_w - 2), a)) ) {
      return 3;
    } else {
      return 2;
    }
  }
  u3_noun
  u3_cwc_cap(
                  u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam, cor))) ||
         (u3_no == u3ud(a)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_cap(a);
    }
  }

