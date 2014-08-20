/* j/4/in_int.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, in, int)(
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2k(u2_nul);
    }
    else if ( u2_nul == b ) {
      return u2k(u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, lr_a;
      u2_noun l_b, n_b, r_b, lr_b;
      u2_noun c;

      if ( (u2_no == u2_cr_cell(a, &n_a, &lr_a)) ) {
        return u2_cm_bail(c3__exit);
      }
      else if ( (u2_no == u2_cr_cell(b, &n_b, &lr_b)) ) {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( u2_yes == j2_mbc(Pt3, vor)(n_b, n_a) ) {
          c = a;    a = b;       b = c;
          c = n_a;  n_a = n_b;   n_b = c;
          c = lr_a; lr_a = lr_b; lr_b = c;
        }
        if ( u2_no == u2_cr_cell(lr_a, &l_a, &r_a) ) {
          return u2_cm_bail(c3__exit);
        }
        else if ( u2_no == u2_cr_cell(lr_b, &l_b, &r_b) ) {
          return u2_cm_bail(c3__exit);
        }
        else if ( u2_yes == u2_cr_sing(n_a, n_b) ) {
          return u2nt(u2k(n_a),
                              j2_mcc(Pt4, in, int)(l_a, l_b),
                              j2_mcc(Pt4, in, int)(r_a, r_b));
        }
        else if ( u2_yes == j2_mbc(Pt3, hor)(n_b, n_a) ) {
          return j2_mcc(Pt4, in, uni)(
                                      j2_mcc(Pt4, in, int)(
                                                          l_a,
                                                          u2nt(
                                                                n_b,
                                                                l_b,
                                                                u2_nul)),
                                      j2_mcc(Pt4, in, int)(
                                                          a,
                                                          r_b));
        }
        else {
          return j2_mcc(Pt4, in, uni)(
                                      j2_mcc(Pt4, in, int)(
                                                          r_a,
                                                          u2nt(
                                                                n_b,
                                                                u2_nul,
                                                                r_b)),
                                      j2_mcc(Pt4, in, int)(
                                                          a,
                                                          l_b));
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, in, int)(
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mcc(Pt4, in, int)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, in, int)[] = {
    { ".2", c3__lite, j2_mc(Pt4, in, int), Tier4, u2_none, u2_none },
    { }
  };


