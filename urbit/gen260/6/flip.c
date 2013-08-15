/* j/6/flip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, flip)(u2_wire wir_r, 
                    u2_noun hel)                                  //  retain
  {
    if ( u2_yes == u2_sing(u2_nock_bone, u2_h(hel)) ) {
      if ( (u2_yes == u2_t(hel)) ) {
        return u2_bc(wir_r, u2_nock_bone, u2_no);
      }
      else {
        c3_assert((u2_no == u2_t(hel)));

        return u2_bc(wir_r, u2_nock_bone, u2_yes);
      }
    }
    else {
      return u2_bq
        (wir_r, u2_nock_trol, 
               u2_rx(wir_r, hel),
               u2_bc(wir_r, u2_nock_bone, u2_no), 
               u2_bc(wir_r, u2_nock_bone, u2_yes));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, flip)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun hel;

    if ( u2_none == (hel = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, flip)(wir_r, hel);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, flip)[] = {
    { ".3", c3__hevy, j2_mb(Pt6, flip), Tier6_a, u2_none, u2_none },
    { }
  };
