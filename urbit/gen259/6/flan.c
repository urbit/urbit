/* j/6/flan.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, flan)(u2_wire wir_r, 
                    u2_noun bos,                                  //  retain
                    u2_noun nif)                                  //  retain
  {
    if ( u2_yes == u2_sing(u2_nock_bone, u2_h(bos)) ) {
      if ( (u2_nul == u2_t(bos)) ) {
        return u2_rx(wir_r, nif);
      }
      else return u2_rx(wir_r, bos);
    }
    else {
      if ( u2_yes == u2_sing(u2_nock_bone, u2_h(nif)) ) {
        if ( (u2_nul == u2_t(nif)) ) {
          return u2_rx(wir_r, bos);
        }
        else return u2_rx(wir_r, nif);
      }
      else {
        return u2_bq
          (wir_r, u2_nock_trol, 
                  u2_rx(wir_r, bos), 
                  u2_rx(wir_r, nif), 
                  u2_bc(wir_r, u2_nock_bone, u2_no));
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, flan)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun bos, nif;

    if ( u2_no == u2_mean(cor, u2_cw_sam_2, &bos, u2_cw_sam_3, &nif, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, flan)(wir_r, bos, nif);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, flan)[] = {
    { ".3", c3__hevy, j2_mb(Pt6, flan), Tier6_a, u2_none, u2_none },
    { }
  };
