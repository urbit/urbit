/* j/2/skim.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, skim)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( _0 == a ) {
      return a;
    }
    else if ( u2_no == u2_dust(a) ) {
      return u2_none;
    } else {
      u2_weak hoz = u2_nk_mung(wir_r, b, u2_rx(wir_r, u2_h(a)));
      u2_weak vyr = j2_mbc(Pt2, skim)(wir_r, u2_t(a), b);

      switch ( hoz ) { 
        case u2_yes:  return u2_rc(wir_r, u2_rx(wir_r, u2_h(a)), vyr);
        case u2_no:   return vyr;
        default:      u2_rl_lose(wir_r, hoz); 
                      u2_rl_lose(wir_r, vyr);
                      return u2_none;
      }
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, skim)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, 8, &a, 9, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt2, skim)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, skim)[] = {
    { ".3", c3__lite, j2_mb(Pt2, skim), Tier2, u2_none, u2_none },
    { }
  };

