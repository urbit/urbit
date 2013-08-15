/* j/3/rip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, rip)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    if ( !u2_fly_is_cat(a) || (a >= 32) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      u2_noun pir = u2_nul;
      c3_g a_g = a;
      c3_w met_w = u2_met(a_g, b);
      c3_w i_w;

      if ( a_g < 5 ) {
        c3_w mek_w = ((1 << (1 << a_g)) - 1);

        for ( i_w = 0; i_w < met_w; i_w++ ) {
          c3_w pat_w = (met_w - (i_w + 1));
          c3_w bit_w = (pat_w << a_g);
          c3_w wor_w = (bit_w >> 5);
          c3_w sif_w = (bit_w & 31);
          c3_w src_w = u2_atom_word(b, wor_w);
          c3_w rip_w = ((src_w >> sif_w) & mek_w);

          pir = u2_rc(wir_r, rip_w, pir);
        }
        return pir;
      }
      else {
        c3_g san_g = (a_g - 5); 
        c3_w san_w = 1 << san_g;

        for ( i_w = 0; i_w < met_w; i_w++ ) {
          c3_w    pat_w = (met_w - (i_w + 1));
          c3_w    wut_w = (pat_w << san_g);
          u2_ray  sal_r = u2_rl_slab(wir_r, san_w);

          if ( 0 == sal_r ) {
            return u2_bl_bail(wir_r, c3__fail);
          } else {
            c3_w    j_w;
            u2_atom rip;

            for ( j_w = 0; j_w < san_w; j_w++ ) {
              *u2_at_ray(sal_r + j_w) = u2_atom_word(b, wut_w + j_w);
            }
            rip = u2_rl_malt(wir_r, sal_r);
            pir = u2_rc(wir_r, rip, pir);
          }
        }
      }
      return pir;
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, rip)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cw_sam_2, &a, u2_cw_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt3, rip)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt3, rip)[] = {
    { ".3", c3__lite, j2_mb(Pt3, rip), Tier3, u2_none, u2_none },
    { }
  };
