/* j/2/need.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, need)(u2_wire wir_r,
                    u2_noun a)                                    //  retain
  {
    if ( _0 == a ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      return u2_rx(wir_r, u2_st(a));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt2, need)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt2, need)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, need)[] = {
    { ".2", c3__lite, j2_mb(Pt2, need), Tier2, u2_none, u2_none },
    { }
  };
