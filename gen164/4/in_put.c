/* j/4/in_put.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, in, put)(u2_wire wir_r,
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2_rt(wir_r, u2_rx(wir_r, b), u2_nul, u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, lr_a;  //  XX copy tree boilerplate to other pt4
      u2_noun c, l_c, n_c, r_c;

      if ( (u2_no == u2_as_cell(a, &n_a, &lr_a)) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else if ( u2_yes == u2_sing(n_a, b) ) {
        return u2_rx(wir_r, a);
      }
      else if ( u2_no == u2_as_cell(lr_a, &l_a, &r_a) ) {
        return u2_bl_bail(wir_r, c3__exit);
      }
      else {
        if ( u2_yes == j2_mbc(Pt3, hor)(wir_r, b, n_a) ) {
          c = j2_mcc(Pt4, in, put)(wir_r, l_a, b);

          if ( u2_yes == j2_mbc(Pt3, vor)(wir_r, n_a, u2_h(c)) ) {
            return u2_rt(wir_r, u2_rx(wir_r, n_a),
                                c,
                                u2_rx(wir_r, r_a));
          }
          else {
            u2_as_trel(c, &n_c, &l_c, &r_c);
            {
              u2_noun d = u2_rt
                (wir_r, u2_rx(wir_r, n_c),
                        u2_rx(wir_r, l_c),
                        u2_rt(wir_r,
                              u2_rx(wir_r, n_a),
                              u2_rx(wir_r, r_c),
                              u2_rx(wir_r, r_a)));

              u2_rl_lose(wir_r, c);
              return d;
            }
          }
        }
        else {
          c = j2_mcc(Pt4, in, put)(wir_r, r_a, b);

          if ( u2_yes == j2_mbc(Pt3, vor)(wir_r, n_a, u2_h(c)) ) {
            return u2_rt(wir_r, u2_rx(wir_r, n_a),
                                u2_rx(wir_r, l_a),
                                c);
          }
          else {
            u2_as_trel(c, &n_c, &l_c, &r_c);
            {
              u2_noun d = u2_rt
                (wir_r, u2_rx(wir_r, n_c),
                        u2_rt(wir_r,
                              u2_rx(wir_r, n_a),
                              u2_rx(wir_r, l_a),
                              u2_rx(wir_r, l_c)),
                        u2_rx(wir_r, r_c));

              u2_rl_lose(wir_r, c);
              return d;
            }
          }
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, in, put)(u2_wire wir_r,
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, in, put)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt4, in, put)[] = {
    { ".2", c3__lite, j2_mc(Pt4, in, put), Tier4, u2_none, u2_none },
    { }
  };
