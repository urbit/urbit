/* j/4/uni.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, by, uni)(u2_wire wir_r,
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == b ) {
      return u2k(a);
    }
    else {
      u2_noun l_b, n_b, r_b;
      u2_noun pn_b, qn_b;

      if ( (u2_no == u2_as_trel(b, &n_b, &l_b, &r_b)) ||
           (u2_no == u2_as_cell(n_b, &pn_b, &qn_b)) )
      {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else {
        u2_noun l = j2_mcc(Pt4, by, uni)(wir_r, a, l_b);
        u2_noun r = j2_mcc(Pt4, by, uni)(wir_r, l, r_b);
        u2_noun z = j2_mcc(Pt4, by, put)(wir_r, r, pn_b, qn_b);

        u2z(l); u2z(r);
        return z;
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, by, uni)(u2_wire wir_r,
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, by, uni)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, by, uni)[] = {
    { ".2", c3__lite, j2_mc(Pt4, by, uni), Tier4, u2_none, u2_none },
    { }
  };
