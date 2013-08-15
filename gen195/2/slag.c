/* j/2/slag.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, slag)(u2_wire wir_r, 
                    u2_atom a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( !u2_fly_is_cat(a) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      c3_w len_w = a;

      while ( len_w ) {
        if ( u2_no == u2_dust(b) ) {
          return u2_bl_bail(wir_r, c3__exit);
        }
        a = u2_t(a);
        len_w--;
      }
      if ( u2_no == u2_dust(b) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      return u2_rx(wir_r, b);
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, slag)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) || 
         (u2_no == u2_stud(a)) ) 
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt2, slag)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, slag)[] = {
    { ".2", c3__lite, j2_mb(Pt2, slag), Tier2, u2_none, u2_none },
    { }
  };
