/* j/4/in_tap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer 
  j2_mcc(Pit, in, tap)(u2_wire wir_r, 
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  transfer
  {
    if ( u2_nul == a ) {
      return u2_rx(wir_r, b);
    }
    else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_as_trel(a, &n_a, &l_a, &r_a)) ) {
        return u2_none;
      }
      else {
        return j2_mcc(Pit, in, tap)
          (wir_r, u2_rx(wir_r, r_a),
                  u2_rc(wir_r, u2_rx(wir_r, n_a),
                               j2_mcc(Pit, in, tap)(wir_r, l_a, b)));
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pit, in, tap)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, 4, &b, 20, &a, 0) ) {
      return u2_none;
    } else {
      return j2_mcc(Pit, in, tap)(wir_r, a, u2_rx(wir_r, b));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, in, tap)[] = {
    { ".3", c3__lite, j2_mc(Pit, in, tap), Tier4, u2_none, u2_none },
    { }
  };
