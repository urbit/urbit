/* j/6/slot.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, slot)(u2_wire wir_r,
                    u2_atom axe,                                  //  retain
                    u2_noun vax)                                  //  retain
  {
    u2_weak fag = u2_frag(axe, u2t(vax));

    fprintf(stderr, "slot axe %d\r\n", axe);

    if ( u2_none == fag ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      u2_noun typ = j2_mcy(Pt6, ut, peek)(
    }
    c3_w i_w, met_w = c3_min(u2_met(3, axe), u2_met(3, vax));

    if ( u2_no == _slot_fiz(wir_r, axe, vax) ) {
      return u2_no;
    }
    for ( i_w = 0; i_w < met_w; i_w++ ) {
      c3_y axe_y = u2_byte(i_w, axe);
      c3_y vax_y = u2_byte(i_w, vax);

      if ( (axe_y >= 'A') && (axe_y <= 'Z') ) axe_y = 0;
      if ( (vax_y >= 'A') && (vax_y <= 'Z') ) vax_y = 0;

      if ( axe_y && vax_y && (axe_y != vax_y) ) {
        return u2_no;
      }
    }
    return u2_yes;
  }

  u2_noun                                                         //  transfer
  j2_mb(Pt6, slot)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun axe, vax;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &axe, u2_cv_sam_3, &vax, 0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_no == u2du(vax)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, slot)(wir_r, axe, vax);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, slot)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, slot), u2_jet_live | u2_jet_test, u2_none, u2_none },
    { }
  };
