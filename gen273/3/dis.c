/* j/3/dis.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, dis)(u2_wire wir_r, 
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
        return u2_none;
      }
      else {
        c3_w i_w;

        u2_chop(5, 0, lna_w, 0, sal_r, a);

        for ( i_w = 0; i_w < lnb_w; i_w++ ) {
          *u2_at_ray(sal_r + i_w) &= u2_atom_word(b, i_w);
        }
        return u2_rl_malt(wir_r, sal_r);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, dis)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, 8, &a, 9, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_none;
    } else {
      return j2_mbc(Pit, dis)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, dis)[] = {
    { ".3", j2_mb(Pit, dis), u2_yes, u2_none, u2_none },
    { }
  };
