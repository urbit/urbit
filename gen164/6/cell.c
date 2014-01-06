/* j/6/cell.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, cell)(u2_wire wir_r, 
                    u2_noun hed,                                  //  retain
                    u2_noun tal)                                  //  retain
  {
    if ( (c3__void == hed) || (c3__void == tal) ) {
      return c3__void;
    } else {
      return u2_bt(wir_r, c3__cell, u2_rx(wir_r, hed), u2_rx(wir_r, tal));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, cell)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun hed, tal;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &hed, u2_cv_sam_3, &tal, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, cell)(wir_r, hed, tal);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, cell)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, cell), Tier6_a, u2_none, u2_none },
    { }
  };
