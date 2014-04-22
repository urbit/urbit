/* j/3/con.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, con)(u2_wire wir_r,
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    c3_w lna_w = u2_met(5, a);
    c3_w lnb_w = u2_met(5, b);

    if ( (lna_w == 0) && (lnb_w == 0) ) {
      return _0;
    } else {
      c3_w   len_w = c3_max(lna_w, lnb_w);
      u2_ray sal_r = u2_rl_slab(wir_r, len_w);

      if ( 0 == sal_r ) {
        return u2_bl_bail(wir_r, c3__fail);
      }
      else {
        c3_w i_w;

        u2_chop(5, 0, lna_w, 0, sal_r, a);

        for ( i_w = 0; i_w < lnb_w; i_w++ ) {
          *u2_at_ray(sal_r + i_w) |= u2_atom_word(b, i_w);
        }
        // return u2_rl_moot(wir_r, sal_r);
        return u2_rl_malt(wir_r, sal_r);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, con)(u2_wire wir_r,
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt3, con)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, con)[] = {
    { ".2", c3__lite, j2_mb(Pt3, con), Tier3, u2_none, u2_none },
    { }
  };
