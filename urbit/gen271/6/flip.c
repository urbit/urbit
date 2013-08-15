/* j/6/flip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pit, flip)(u2_wire wir_r, 
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
  j2_mb(Pit, flip)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun hel;

    if ( u2_none == (hel = u2_frag(4, cor)) ) {
      return u2_bl_bail(wir_r);
    } else {
      return j2_mby(Pit, flip)(wir_r, hel);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, flip)[] = {
    { ".3", c3__hevy, j2_mb(Pit, flip), Tier6_a, u2_none, u2_none },
    { }
  };
