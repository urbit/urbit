/* j/2/flop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, flop)(u2_wire wir_r,
                    u2_noun a)                                    //  retain
  {
    u2_weak b = _0;

    while ( 1 ) {
      if ( u2_nul == a ) {
        return b;
      }
      else if ( u2_no == u2_dust(a) ) {
        u2_rl_lose(wir_r, b);

        return u2_bl_bail(wir_r, c3__exit);
      }
      else {
        b = u2_rc(wir_r, u2_rx(wir_r, u2_h(a)), b);
        a = u2_t(a);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt2, flop)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt2, flop)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, flop)[] = {
    { ".2", c3__lite, j2_mb(Pt2, flop), Tier2, u2_none, u2_none },
    { }
  };
