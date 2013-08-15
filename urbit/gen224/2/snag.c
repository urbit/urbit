/* j/2/snag.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, snag)(u2_wire wir_r, 
                    u2_atom a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( !u2_fly_is_cat(a) ) {
      return u2_none;
    }
    else {
      c3_w len_w = a;

      while ( len_w ) {
        if ( u2_no == u2_dust(b) ) {
          return u2_none;
        }
        b = u2_t(b);
        len_w--;
      }
      if ( u2_no == u2_dust(b) ) {
        return u2_none;
      }
      return u2_rx(wir_r, u2_h(b));
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, snag)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cw_sam_2, &a, u2_cw_sam_3, &b, 0)) || 
         (u2_no == u2_stud(a)) ) 
    {
      return u2_none;
    } else {
      return j2_mbc(Pt2, snag)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt2, snag)[] = {
    { ".3", c3__lite, j2_mb(Pt2, snag), Tier2, u2_none, u2_none },
    { }
  };
