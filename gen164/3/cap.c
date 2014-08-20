/* j/3/cap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, cap)(
                   u2_atom a)                                     //  retain
  {
    c3_w met_w = u2_cr_met(0, a);

    if ( met_w < 2 ) {
      return u2_cm_bail(c3__exit);
    }
    else if ( (1 == u2_cr_bit((met_w - 2), a)) ) {
      return 3;
    } else {
      return 2;
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, cap)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, cap)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, cap)[] = {
    { ".2", c3__lite, j2_mb(Pt3, cap), Tier3, u2_none, u2_none },
    { }
  };
