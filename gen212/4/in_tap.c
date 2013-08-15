/* j/4/in_tap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_weak                                                  //  produce
  _tap_in(u2_wire wir_r,
          u2_noun a,                                              //  retain
          u2_noun b)                                              //  submit
  {
    if ( u2_nul == a ) {
      return b;
    } else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_as_trel(a, &n_a, &l_a, &r_a)) ) {
        u2_rz(wir_r, b);
        return u2_bl_bail(wir_r, c3__exit);
      } else {
        return _tap_in
          (wir_r, r_a, 
                  u2_rc(wir_r, u2_rx(wir_r, n_a), 
                               _tap_in(wir_r, l_a, b)));
      }
    } 
  }

  u2_weak                                                         //  produce 
  j2_mcc(Pt4, in, tap)(u2_wire wir_r, 
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    return _tap_in(wir_r, a, u2_rx(wir_r, b));
  }
  u2_weak                                                         //  produce
  j2_mc(Pt4, in, tap)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cw_con_sam, &a, u2_cw_sam, &b, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, in, tap)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt4, in, tap)[] = {
    { ".3", c3__lite, j2_mc(Pt4, in, tap), Tier4, u2_none, u2_none },
    { }
  };
