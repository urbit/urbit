/* j/3/jam.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _jam_in(u2_wire, u2_atom, u2_atom, u2_noun, u2_noun);

  static u2_noun                                                  //  produce
  _jam_in_pair(u2_wire wir_r,
               u2_atom h_a,                                       //  retain
               u2_atom t_a,                                       //  retain
               u2_atom b,                                         //  retain
               u2_noun m,                                         //  retain
               u2_noun l)                                         //  retain
  {
    u2_noun w = u2_bc(wir_r, u2_bc(wir_r, _2, _1), u2_rx(wir_r, l));
    u2_noun x = j2_mbc(Pt1, add)(wir_r, _2, b);
    u2_noun d = _jam_in(wir_r, h_a, x, m, w);
    u2_noun p_d, q_d, r_d;
    u2_noun r;

    u2_as_trel(d, &p_d, &q_d, &r_d);
    {
      u2_noun y = j2_mbc(Pt1, add)(wir_r, x, p_d);
      u2_noun e = _jam_in(wir_r, t_a, y, r_d, q_d);
      u2_noun p_e, q_e, r_e;

      u2_as_trel(e, &p_e, &q_e, &r_e);
      {
        u2_noun z = j2_mbc(Pt1, add)(wir_r, p_d, p_e);

        r = u2_bt
          (wir_r, j2_mbc(Pt1, add)(wir_r, _2, z),
                  u2_rx(wir_r, q_e),
                  u2_rx(wir_r, r_e));

        u2_rz(wir_r, z);
      }
      u2_rz(wir_r, e);
      u2_rz(wir_r, y);
    }
    u2_rz(wir_r, d);
    u2_rz(wir_r, x);
    u2_rz(wir_r, w);

    return r;
  }

  static u2_noun                                                  //  produce
  _jam_in_flat(u2_wire wir_r,
               u2_atom a,                                         //  retain
               u2_noun m,                                         //  retain
               u2_noun l)                                         //  retain
  {
    u2_noun d = j2_mby(Pt5, mat)(wir_r, a);
    u2_noun x = j2_mbc(Pt1, add)(wir_r, _1, u2_h(d));
    u2_noun y = u2_bt
      (wir_r, u2_rx(wir_r, x),
              u2_bc(wir_r, u2_bc(wir_r, 
                                 x,
                                 j2_mbc(Pt3, lsh)(wir_r, _0, _1, u2_t(d))),
                           u2_rx(wir_r, l)),
              u2_rx(wir_r, m));

    u2_rz(wir_r, d);

    return y;
  }

  static u2_noun                                                  //  produce
  _jam_in_ptr(u2_wire wir_r,
              u2_atom u_c,                                        //  retain
              u2_noun m,                                          //  retain
              u2_noun l)                                          //  retain
  {
    u2_noun d = j2_mby(Pt5, mat)(wir_r, u_c);
    u2_atom x = j2_mbc(Pt3, lsh)(wir_r, _0, _2, u2_t(d));
    u2_atom y = j2_mbc(Pt1, add)(wir_r, _2, u2_h(d));
    u2_noun z = u2_bt
      (wir_r, u2_rx(wir_r, y),
              u2_bc(wir_r, u2_bc(wir_r, y, j2_mbc(Pt3, mix)(wir_r, _3, x)),
                           u2_rx(wir_r, l)),
              u2_rx(wir_r, m));

    u2_rz(wir_r, d);
    u2_rz(wir_r, x);

    return z;
  }

  static u2_noun                                                  //  produce
  _jam_in(u2_wire wir_r,
          u2_atom a,                                              //  retain
          u2_atom b,                                              //  retain
          u2_noun m,                                              //  retain
          u2_noun l)                                              //  retain
  {
    u2_noun c = j2_mcc(Pt4, by, get)(wir_r, m, a);
    u2_noun x;

    if ( u2_nul == c ) {
      m = j2_mcc(Pt4, by, put)(wir_r, m, a, b);

      if ( u2_yes == u2_stud(a) ) {
        x = _jam_in_flat(wir_r, a, m, l);
      } else {
        x = _jam_in_pair(wir_r, u2_h(a), u2_t(a), b, m, l);
      }
      u2_rz(wir_r, m);
    }
    else {
      u2_noun u_c = u2_t(c);

      if ( u2_yes == u2_stud(a) && u2_met(0, a) <= u2_met(0, u_c) ) {
        x = _jam_in_flat(wir_r, a, m, l);
      }
      else {
        x = _jam_in_ptr(wir_r, u_c, m, l);
      }
      u2_rz(wir_r, c);
    }
    return x;
  }

  u2_noun                                                         //  transfer
  j2_mby(Pt5, jam)(u2_wire wir_r, 
                   u2_atom a)                                     //  retain
  {
    u2_noun x = _jam_in(wir_r, a, _0, u2_nul, u2_nul);
    u2_noun q = j2_mbc(Pt2, flop)(wir_r, u2_h(u2_t(x)));
    u2_noun r = j2_mbc(Pt3, can)(wir_r, _0, q);

    u2_rz(wir_r, x);
    u2_rz(wir_r, q);

    return r;
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, jam)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cw_sam, cor))) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt5, jam)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, jam)[] = {
    { ".3", c3__hevy, j2_mb(Pt5, jam), Tier3, u2_none, u2_none },
    { }
  };
