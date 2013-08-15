/* j/6/cell.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pit, cell)(u2_wire wir_r, 
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
  j2_mb(Pit, cell)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun hed, tal;

    if ( u2_no == u2_mean(cor, 8, &hed, 9, &tal, 0) ) {
      return u2_bl_bail(wir_r);
    } else {
      return j2_mby(Pit, cell)(wir_r, hed, tal);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, cell)[] = {
    { ".3", c3__hevy, j2_mb(Pit, cell), Tier6_a, u2_none, u2_none },
    { }
  };
