/* j/4/in_tap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_weak                                                  //  produce
  _tap_in(
          u2_noun a,                                              //  retain
          u2_noun b)                                              //  submit
  {
    if ( u2_nul == a ) {
      return b;
    } else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_cr_trel(a, &n_a, &l_a, &r_a)) ) {
        u2z(b);
        return u2_cm_bail(c3__exit);
      } else {
        return _tap_in
          (r_a,
                  u2nc(u2k(n_a),
                               _tap_in(l_a, b)));
      }
    }
  }

  u2_weak                                                         //  produce
  j2_mcc(Pt4, in, tap)(
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    return _tap_in(a, u2k(b));
  }
  u2_weak                                                         //  produce
  j2_mc(Pt4, in, tap)(
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mcc(Pt4, in, tap)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, in, tap)[] = {
    { ".2", c3__lite, j2_mc(Pt4, in, tap), Tier4, u2_none, u2_none },
    { }
  };
