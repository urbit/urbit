/* j/2/turn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, turn)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( _0 == a ) {
      return a;
    } 
    else if ( u2_no == u2_dust(a) ) {
      return u2_none;
    }
    else {   
      return u2_rc
        (wir_r,
         u2_nk_mong(wir_r, b, u2_rx(wir_r, u2_h(a))),
         j2_mbc(Pt2, turn)(wir_r, u2_t(a), b));
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, turn)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cw_sam_2, &a, u2_cw_sam_3, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt2, turn)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, turn)[] = {
    { ".3", c3__lite, j2_mb(Pt2, turn), Tier2, u2_none, u2_none },
    { }
  };
