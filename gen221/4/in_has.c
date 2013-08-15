/* j/4/in_has.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_flag 
  j2_mcc(Pt4, in, has)(u2_wire wir_r, 
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2_no;
    }
    else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_mean(a, 2, &n_a, 6, &l_a, 7, &r_a, 0)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else {
        if ( (u2_yes == u2_sing(b, n_a)) ) {
          return u2_yes;
        } 
        else {
          if ( u2_yes == j2_mbc(Pt3, hor)(wir_r, b, n_a) ) {
            return j2_mcc(Pt4, in, has)(wir_r, l_a, b);
          } 
          else return j2_mcc(Pt4, in, has)(wir_r, r_a, b);
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, in, has)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cw_con_sam, &a, u2_cw_sam, &b, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, in, has)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt4, in, has)[] = {
    { ".3", c3__lite, j2_mc(Pt4, in, has), Tier4, u2_none, u2_none },
    { }
  };
