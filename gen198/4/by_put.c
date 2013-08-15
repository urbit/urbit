/* j/4/put.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mcc(Pt4, by, put)(u2_wire wir_r, 
                       u2_noun a,                                 //  retain
                       u2_noun b,                                 //  retain
                       u2_noun c)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2_rt(wir_r, 
                   u2_rc(wir_r, u2_rx(wir_r, b), 
                                u2_rx(wir_r, c)),
                   u2_nul,
                   u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, pn_a, qn_a;
      u2_noun d, l_d, n_d, r_d;

      if ( (u2_no == u2_as_trel(a, &n_a, &l_a, &r_a)) ||
           (u2_no == u2_as_cell(n_a, &pn_a, &qn_a)) )
      {
        return u2_bl_bail(wir_r, c3__exit);
      } 
      else if ( u2_yes == u2_sing(pn_a, b) ) {
        if ( u2_yes == u2_sing(qn_a, c) ) {
          return u2_rx(wir_r, a);
        } else {
          return u2_rt
            (wir_r, u2_rc(wir_r, u2_rx(wir_r, b), u2_rx(wir_r, c)),
                    u2_rx(wir_r, l_a),
                    u2_rx(wir_r, r_a));
        }
      }
      else {
        if ( u2_yes == j2_mbc(Pt3, gor)(wir_r, b, pn_a) ) {
          d = j2_mcc(Pt4, by, put)(wir_r, l_a, b, c);

          if ( u2_yes == j2_mbc(Pt3, vor)(wir_r, pn_a, u2_h(u2_h(d))) ) {
            return u2_rt(wir_r, u2_rx(wir_r, n_a), 
                                d, 
                                u2_rx(wir_r, r_a));
          }
          else {
            u2_as_trel(d, &n_d, &l_d, &r_d);
            {
              u2_noun e = u2_rt
                (wir_r, u2_rx(wir_r, n_d),
                        u2_rx(wir_r, l_d),
                        u2_rt(wir_r,
                              u2_rx(wir_r, n_a),
                              u2_rx(wir_r, r_d),
                              u2_rx(wir_r, r_a)));

              u2_rl_lose(wir_r, d);
              return e;
            }
          }
        }
        else {
          d = j2_mcc(Pt4, by, put)(wir_r, r_a, b, c);

          if ( u2_yes == j2_mbc(Pt3, vor)(wir_r, pn_a, u2_h(u2_h(d))) ) {
            return u2_rt(wir_r, u2_rx(wir_r, n_a),
                                u2_rx(wir_r, l_a), 
                                d);
          }
          else {
            u2_as_trel(d, &n_d, &l_d, &r_d);
            {
              u2_noun e = u2_rt
                (wir_r, u2_rx(wir_r, n_d),
                        u2_rt(wir_r,
                              u2_rx(wir_r, n_a),
                              u2_rx(wir_r, l_a),
                              u2_rx(wir_r, l_d)),
                        u2_rx(wir_r, r_d));

              u2_rl_lose(wir_r, d);
              return e;
            }
          }
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt4, by, put)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b, c;

    if ( u2_no == u2_mean(cor, u2_cw_con_sam, &a, 
                               u2_cw_sam_2,   &b, 
                               u2_cw_sam_3,   &c, 0) ) {
                               
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mcc(Pt4, by, put)(wir_r, a, b, c);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt4, by, put)[] = {
    { ".3", c3__lite, j2_mc(Pt4, by, put), Tier4, u2_none, u2_none },
    { }
  };
