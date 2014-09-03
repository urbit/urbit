/* j/4/gas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqdi_gas(
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == b ) {
      return u2k(a);
    }
    else {
      if ( u2_no == u2du(b) ) {
        return u2_cm_bail(c3__exit);
      } else {
        u2_noun i_b = u2h(b);
        u2_noun t_b = u2t(b);
        u2_noun c;

        if ( u2_none == (c = u2_cqdi_put(a, i_b)) ) {
          return u2_cm_bail(c3__exit);
        } else {
          u2_noun d = u2_cqdi_gas(c, t_b);

          u2z(c);
          return d;
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, in, gas)(
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_gas(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, in, gas)[] = {
    { ".2", c3__lite, j2_mc(Pt4, in, gas), Tier4, u2_none, u2_none },
    { }
  };
