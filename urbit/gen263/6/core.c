/* j/6/core.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pit, core)(u2_wire wir_r, 
                    u2_noun pac,                                  //  retain
                    u2_noun con)                                  //  retain
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2_bt(wir_r, c3__core, u2_rx(wir_r, pac), u2_rx(wir_r, con));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pit, core)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun pac, con;

    if ( u2_no == u2_mean(cor, 8, &pac, 9, &con, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pit, core)(wir_r, pac, con);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, core)[] = {
    { ".3", c3__hevy, j2_mb(Pit, core), Tier6_a, u2_none, u2_none },
    { }
  };
