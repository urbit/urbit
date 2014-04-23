/* j/3/vor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, vor)(u2_wire wir_r,
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    c3_w c_w = u2_mug(u2_mug(a));
    c3_w d_w = u2_mug(u2_mug(b));

    if ( c_w == d_w ) {
      return j2_mbc(Pt3, dor)(wir_r, a, b);
    }
    else return (c_w < d_w) ? u2_yes : u2_no;
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, vor)(u2_wire wir_r,
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt3, vor)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, vor)[] = {
    { ".2", c3__lite, j2_mb(Pt3, vor), Tier3, u2_none, u2_none },
    { }
  };
