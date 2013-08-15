/* j/2/bind.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, bind)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( _0 == a ) {
      return _0;
    } else {
      return u2_ru(wir_r, u2_nk_mung(wir_r, b, u2_rx(wir_r, u2_st(a))));
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, bind)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, 8, &a, 9, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt2, bind)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, bind)[] = {
    { ".3", c3__lite, j2_mb(Pt2, bind), Tier2, u2_none, u2_none },
    { }
  };

