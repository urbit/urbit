/* j/2/drop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, drop)(u2_wire wir_r, 
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
  j2_mb(Pit, drop)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_frag(4, cor)) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, drop)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, drop)[] = {
    { ".3", j2_mb(Pit, drop), u2_yes, u2_none, u2_none },
    { }
  };

