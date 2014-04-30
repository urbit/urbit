/* j/3/cue.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_ray T_r;

  static u2_noun                                                  //  produce
  _cue_in(u2_wire wir_r,
          u2_atom a,                                              //  retain
          u2_atom b,                                              //  retain
          u2_noun m)                                              //  retain
  {
    u2_noun p, q, r;

    if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, b, 1, a) ) {
      u2_noun x = j2_mbc(Pt1, inc)(wir_r, b);
      u2_noun c = j2_mby(Pt5, rub)(wir_r, x, a);

      p = j2_mbc(Pt1, inc)(wir_r, u2_h(c));
      q = u2_rx(wir_r, u2_t(c));
      // q = u2_cs_save(wir_r, T_r, 0, b, q);
      r = j2_mcc(Pt4, by, put)(wir_r, m, b, u2_t(c));

      u2_rz(wir_r, c);
      u2_rz(wir_r, x);
    }
    else {
      u2_noun c = j2_mbc(Pt1, add)(wir_r, _2, b);
      u2_noun l = j2_mbc(Pt1, inc)(wir_r, b);

      if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, l, 1, a) ) {
        u2_noun u, v, w;
        u2_noun x, y;

        u = _cue_in(wir_r, a, c, m);
        x = j2_mbc(Pt1, add)(wir_r, u2_h(u), c);
        v = _cue_in(wir_r, a, x, u2_t(u2_t(u)));

        w = u2_bc(wir_r, u2_rx(wir_r, u2_h(u2_t(u))),
                         u2_rx(wir_r, u2_h(u2_t(v))));

        y = j2_mbc(Pt1, add)(wir_r, u2_h(u), u2_h(v));

        p = j2_mbc(Pt1, add)(wir_r, _2, y);
        q = w;
        // q = u2_cs_save(wir_r, T_r, 0, b, w);
        r = j2_mcc(Pt4, by, put)(wir_r, u2_t(u2_t(v)), b, w);

        u2_rz(wir_r, u); u2_rz(wir_r, v); u2_rz(wir_r, x); u2_rz(wir_r, y);
      }
      else {
        u2_noun d = j2_mby(Pt5, rub)(wir_r, c, a);
        u2_noun x = j2_mcc(Pt4, by, get)(wir_r, m, u2_t(d));
        // u2_weak T = u2_cs_find(wir_r, T_r, 0, u2_t(d));

        p = j2_mbc(Pt1, add)(wir_r, _2, u2_h(d));
        if ( u2_nul == x ) {
          return u2_bl_bail(wir_r, c3__exit);
        }
#if 0
        else if ( (u2_none == T) || (u2_no == u2_sing(T, x)) ) {
          printf("fail!\n");
          return u2_bl_bail(wir_r, c3__fail);
        }
#endif
        q = u2_rx(wir_r, u2_t(x));
        r = u2_rx(wir_r, m);

        u2_rz(wir_r, x);
        u2_rz(wir_r, d);
      }
      u2_rz(wir_r, l);
      u2_rz(wir_r, c);
    }
    return u2_bt(wir_r, p, q, r);
  }

  u2_noun                                                         //  transfer
  j2_mby(Pt5, cue)(u2_wire wir_r,
                   u2_atom a)                                     //  retain
  {
#if 0
    if ( 0 == T_r ) {
      T_r = u2_cs_make(wir_r);
    }
#endif
    {
      u2_noun x = _cue_in(wir_r, a, _0, u2_nul);
      u2_noun y = u2_rx(wir_r, u2_h(u2_t(x)));

      u2_rz(wir_r, x);
      return y;
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, cue)(u2_wire wir_r,
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cv_sam, cor))) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt5, cue)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, cue)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, cue), Tier3, u2_none, u2_none },
    { }
  };
