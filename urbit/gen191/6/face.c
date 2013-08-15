/* j/6/face.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, face)(u2_wire wir_r, 
                    u2_noun cog,                                  //  retain
                    u2_noun tip)                                  //  retain
  {
    if ( c3__void == tip ) {
      return c3__void;
    }
    else return u2_bt
      (wir_r, c3__face, u2_rx(wir_r, cog), u2_rx(wir_r, tip));
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, face)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun cog, tip;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &cog, u2_cv_sam_3, &tip, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, face)(wir_r, cog, tip);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, face)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, face), Tier6_a, u2_none, u2_none },
    { }
  };
