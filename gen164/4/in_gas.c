/* j/4/gas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, in, gas)(u2_wire wir_r,
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == b ) {
      return u2_rx(wir_r, a);
    }
    else {
      if ( u2_no == u2_dust(b) ) {
        return u2_bl_bail(wir_r, c3__exit);
      } else {
        u2_noun i_b = u2_h(b);
        u2_noun t_b = u2_t(b);
        u2_noun c;

        if ( u2_none == (c = j2_mcc(Pt4, in, put)(wir_r, a, i_b)) ) {
          return u2_bl_bail(wir_r, c3__exit);
        } else {
          u2_noun d = j2_mcc(Pt4, in, gas)(wir_r, c, t_b);

          u2_rl_lose(wir_r, c);
          return d;
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, in, gas)(u2_wire wir_r,
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, in, gas)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, in, gas)[] = {
    { ".2", c3__lite, j2_mc(Pt4, in, gas), Tier4, u2_none, u2_none },
    { }
  };
