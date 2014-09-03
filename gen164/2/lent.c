/* j/2/lent.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqb_lent(
                    u2_noun a)                                    //  retain
  {
    u2_weak len = 0;

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
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_lent(a);
    }
  }
