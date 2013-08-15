/* j/2/skip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, skip)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( _0 == a ) {
      return a;
    }
    else if ( u2_no == u2_dust(a) ) {
      return u2_none;
    } else {
      u2_weak hoz = u2_nk_mong(wir_r, b, u2_rx(wir_r, u2_h(a)));
      u2_weak vyr = j2_mbc(Pt2, skip)(wir_r, u2_t(a), b);

      switch ( hoz ) { 
        case u2_yes:  return vyr;
        case u2_no:   return u2_rc(wir_r, u2_rx(wir_r, u2_h(a)), vyr);
        default:      u2_rl_lose(wir_r, hoz); 
                      u2_rl_lose(wir_r, vyr);
                      return u2_none;
      }
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, skip)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt2, skip)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, skip)[] = {
    { ".2", c3__lite, j2_mb(Pt2, skip), Tier2, u2_none, u2_none },
    { }
  };
