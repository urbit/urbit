/* j/2/drop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, drop)(u2_wire wir_r, 
                    u2_noun a)                                    //  retain
  {
    if ( _0 == a ) {
      return u2_nul;
    }
    else {
      return u2_ro(wir_r, u2_rx(wir_r, u2_st(a)));
    }
  }        
  u2_noun                                                         //  transfer
  j2_mb(Pt2, drop)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_frag(u2_cv_sam, cor)) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt2, drop)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, drop)[] = {
    { ".3", c3__lite, j2_mb(Pt2, drop), Tier2, u2_none, u2_none },
    { }
  };

