/* j/3/mas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, mas)(
                   u2_atom a)                                     //  retain
  {
    c3_w b_w;
    u2_atom c, d, e, f;

    b_w = u2_cr_met(0, a);
    if ( b_w < 2 ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      c = j2_mbc(Pt3, bex)((b_w - 1));
      d = j2_mbc(Pt3, bex)((b_w - 2));
      e = j2_mbc(Pt1, sub)(a, c);
      f = j2_mbc(Pt3, con)(e, d);

      u2z(c);
      u2z(d);
      u2z(e);

      return f;
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, mas)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, mas)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, mas)[] = {
    { ".2", c3__lite, j2_mb(Pt3, mas), Tier3, u2_none, u2_none },
    { }
  };
