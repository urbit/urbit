/* j/2/clap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, clap)(u2_wire wir_r,  
                    u2_noun a,                                    //  retain
                    u2_noun b,                                    //  retain
                    u2_noun c)                                    //  retain
  {
    if ( _0 == a ) {
      return u2_rx(wir_r, b);
    } 
    else if ( _0 == b ) {
      return u2_rx(wir_r, a);
    }
    else {
      return u2_ru
        (wir_r,
         u2_nk_mung(wir_r, c, u2_rc(wir_r, u2_rx(wir_r, u2_st(a)),
                                           u2_rx(wir_r, u2_st(b)))));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pit, clap)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun a, b, c;

    if ( u2_no == u2_mean(cor, 8, &a, 18, &b, 19, &c, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, clap)(wir_r, a, b, c);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, clap)[] = {
    { ".3", j2_mb(Pit, clap), u2_no, u2_none, u2_none },
    { }
  };
