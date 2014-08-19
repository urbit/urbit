/* j/2/lent.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, lent)(
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
  j2_mb(Pt2, lent)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, lent)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, lent)[] = {
    { ".2", c3__lite, j2_mb(Pt2, lent), Tier2, u2_none, u2_none },
    { }
  };
