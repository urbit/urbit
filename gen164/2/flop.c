/* j/2/flop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, flop)(
                    u2_noun a)                                    //  retain
  {
    u2_weak b = 0;

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
  u2_weak                                                         //  transfer
  j2_mb(Pt2, flop)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, flop)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, flop)[] = {
    { ".2", c3__lite, j2_mb(Pt2, flop), Tier2, u2_none, u2_none },
    { }
  };
