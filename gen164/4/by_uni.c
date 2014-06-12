/* j/4/by_uni.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, by, uni)(u2_wire wir_r,
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2_rx(wir_r, b);
    }
    else if ( u2_nul == b ) {
      return u2_rx(wir_r, a);
    }
    else {
      u2_noun l_a, n_a, r_a, lr_a, p_n_a, q_n_a;
      u2_noun l_b, n_b, r_b, lr_b, p_n_b, q_n_b;

      if ( (u2_no == u2_as_cell(a, &n_a, &lr_a)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( (u2_no == u2_as_cell(b, &n_b, &lr_b)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( (u2_no == u2_as_cell(n_a, &p_n_a, &q_n_a)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( (u2_no == u2_as_cell(n_b, &p_n_b, &q_n_b)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( u2_no == u2_as_cell(lr_a, &l_a, &r_a) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( u2_no == u2_as_cell(lr_b, &l_b, &r_b) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( u2_yes == j2_mbc(Pt3, vor)(wir_r, p_n_a, p_n_b) ) {
        if ( u2_yes == u2_sing(p_n_a, p_n_b) ) {
          return u2_rt(wir_r, n_b,
                              j2_mcc(Pt4, by, uni)(
                                wir_r, u2_rx(wir_r, l_a), u2_rx(wir_r, l_b)),
                              j2_mcc(Pt4, by, uni)(
                                wir_r, u2_rx(wir_r, r_a), u2_rx(wir_r, r_b)));
        }
        else if ( u2_yes == j2_mbc(Pt3, hor)(wir_r, p_n_b, p_n_a) ) {
          return j2_mcc(Pt4, by, uni)(
            wir_r,
            u2_rt(wir_r,
                  u2_rx(wir_r, n_a),
                  j2_mcc(Pt4, by, uni)(wir_r,
                                      u2_rx(wir_r, l_a),
                                      u2_rt(wir_r,
                                            u2_rx(wir_r, n_b),
                                            u2_rx(wir_r, l_b),
                                            u2_rx(wir_r, u2_nul))),
                  u2_rx(wir_r, r_a)),
            u2_rx(wir_r, r_b));
        }
        else {
          return j2_mcc(Pt4, by, uni)(
            wir_r,
            u2_rt(wir_r,
                  u2_rx(wir_r, n_a),
                  u2_rx(wir_r, l_a),
                  j2_mcc(Pt4, by, uni)(wir_r,
                                      u2_rx(wir_r, r_a),
                                      u2_rt(wir_r,
                                            u2_rx(wir_r, n_b),
                                            u2_rx(wir_r, u2_nul),
                                            u2_rx(wir_r, r_b)))),
            u2_rx(wir_r, l_b));
        }
      }
      else if ( u2_yes == u2_sing(p_n_b, p_n_a) ) {
        return u2_rt(
          wir_r,
          u2_rx(wir_r, n_b),
          j2_mcc(Pt4, by, uni)(wir_r, u2_rx(wir_r, r_b), u2_rx(wir_r, r_a)),
          j2_mcc(Pt4, by, uni)(wir_r, u2_rx(wir_r, l_b), u2_rx(wir_r, l_a)));
      }
      else if ( u2_yes == j2_mbc(Pt3, hor)(wir_r, p_n_a, p_n_b) ) {
        return j2_mcc(Pt4, by, uni)(
          wir_r,
          u2_rx(wir_r, r_a),
          u2_rt(wir_r,
                u2_rx(wir_r, n_b),
                j2_mcc(Pt4, by, uni)(wir_r,
                                    u2_rt(wir_r,
                                          u2_rx(wir_r, n_a),
                                          u2_rx(wir_r, l_a),
                                          u2_rx(wir_r, u2_nul)),
                                    u2_rx(wir_r, l_b)),
                u2_rx(wir_r, r_b)));
      }
      else {
        return j2_mcc(Pt4, by, uni)(
          wir_r,
          u2_rx(wir_r, l_a),
          u2_rt(wir_r,
                u2_rx(wir_r, n_b),
                u2_rx(wir_r, l_b),
                j2_mcc(Pt4, by, uni)(wir_r,
                                    u2_rx(wir_r, r_b),
                                    u2_rt(wir_r,
                                          u2_rx(wir_r, n_a),
                                          u2_rx(wir_r, u2_nul),
                                          u2_rx(wir_r, r_a)))));
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, by, uni)(u2_wire wir_r,
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, by, uni)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, by, uni)[] = {
    { ".2", c3__lite, j2_mc(Pt4, by, uni), Tier4, u2_none, u2_none },
    { }
  };

