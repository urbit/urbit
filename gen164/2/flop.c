/* j/2/flop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqb_flop(
                    u2_noun a)
  {
    u2_noun b = 0;

    while ( 1 ) {
      if ( u2_nul == a ) {
        return b;
      }
      else if ( u2_no == u2du(a) ) {
        u2z(b);

        return u2_cm_bail(c3__exit);
      }
      else {
        b = u2nc(u2k(u2h(a)), b);
        a = u2t(a);
      }
    }
  }
  u2_noun
  u2_cwb_flop(
                   u2_noun cor)
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_flop(a);
    }
  }
