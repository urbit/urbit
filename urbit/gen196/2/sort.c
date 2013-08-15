/* j/2/sort.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, sort)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    //  must think about
    //
    return u2_bl_bail(wir_r, c3__fail);
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, sort)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt2, sort)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, sort)[] = {
    { ".2", c3__lite, u2_jet_dead, Tier2, u2_none, u2_none },
    { }
  };
