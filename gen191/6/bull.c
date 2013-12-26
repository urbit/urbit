/* j/6/bull.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, bull)(u2_wire wir_r, 
                    u2_noun bid,                                  //  retain
                    u2_noun der)                                  //  retain
  {
    if ( c3__void == der ) {
      return c3__void;
    }
    else return u2_bt
      (wir_r, c3__bull, u2_rx(wir_r, bid), u2_rx(wir_r, der));
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, bull)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun bid, der;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &bid, u2_cv_sam_3, &der, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, bull)(wir_r, bid, der);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, bull)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, bull), Tier6_a, u2_none, u2_none },
    { }
  };
