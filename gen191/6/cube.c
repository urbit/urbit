/* j/6/cube.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, cube)(u2_wire wir_r, 
                    u2_noun dil,                                  //  retain
                    u2_noun goq)                                  //  retain
  {
    if ( c3__void == goq ) {
      return c3__void;
    }
    else return u2_bt
      (wir_r, c3__cube, u2_rx(wir_r, dil), u2_rx(wir_r, goq));
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, cube)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun dil, goq;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &dil, u2_cv_sam_3, &goq, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, cube)(wir_r, dil, goq);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, cube)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, cube), Tier6_a, u2_none, u2_none },
    { }
  };
