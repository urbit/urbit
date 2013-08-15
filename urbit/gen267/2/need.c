/* j/2/need.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, need)(u2_wire wir_r, 
                    u2_noun a)                                    //  retain
  {
    if ( _0 == a ) {
      return u2_none;
    }
    else {
      return u2_rx(wir_r, u2_st(a));
    }
  }        
  u2_noun                                                         //  transfer
  j2_mb(Pit, need)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_frag(4, cor)) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, need)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, need)[] = {
    { ".3", c3__lite, j2_mb(Pit, need), Tier2, u2_none, u2_none },
    { }
  };
