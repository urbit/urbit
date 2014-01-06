/* j/3/rap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, rap)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_noun b)                                     //  retain
  {
    if ( !u2_fly_is_cat(a) || (a >= 32) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      c3_g   a_g = a;
      c3_w   tot_w = 0;
      u2_ray sal_r;
     
      /* Measure and validate the slab required.
      */
      {
        u2_noun cab = b;

        while ( 1 ) {
          u2_noun h_cab;
          c3_w    len_w;

          if ( _0 == cab ) {
            break;
          }
          else if ( u2_no == u2_dust(cab) ) {
            return u2_bl_bail(wir_r, c3__exit);
          }
          else if ( u2_no == u2_stud(h_cab = u2_h(cab)) ) {
            return u2_bl_bail(wir_r, c3__exit);
          }
          else if ( (tot_w + (len_w = u2_met(a_g, h_cab))) < tot_w ) {
            return u2_bl_bail(wir_r, c3__fail);
          }
          tot_w += len_w;
          cab = u2_t(cab); 
        }
        if ( 0 == tot_w ) {
          return _0;
        }
        if ( 0 == (sal_r = u2_rl_slaq(wir_r, a_g, tot_w)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
      }

      /* Chop the list atoms in.
      */
      {
        u2_noun cab = b;
        c3_w    pos_w = 0;

        while ( _0 != cab ) {
          u2_noun h_cab = u2_h(cab);
          c3_w    len_w = u2_met(a_g, h_cab);

          u2_chop(a_g, 0, len_w, pos_w, sal_r, h_cab);
          pos_w += len_w;
          cab = u2_t(cab);
        }
      }
      // return u2_rl_moot(wir_r, sal_r);
      return u2_rl_malt(wir_r, sal_r);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, rap)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt3, rap)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt3, rap)[] = {
    { ".2", c3__lite, j2_mb(Pt3, rap), Tier3, u2_none, u2_none },
    { }
  };
