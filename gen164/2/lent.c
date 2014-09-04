/* j/2/lent.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_lent(
                    u2_noun a)
  {
    u2_noun len = 0;

    while ( 1 ) {
      if ( 0 == a ) {
        return len;
      }
      else if ( u2_no == u2du(a) ) {
        u2z(len);
        return u2_cm_bail(c3__exit);
      }
      else {
        len = u2_ci_vint(len);
        a = u2t(a);
      }
    }
  }
  u2_noun
  u2_cwb_lent(
                   u2_noun cor)
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_lent(a);
    }
  }
