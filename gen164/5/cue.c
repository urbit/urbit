/* j/3/cue.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"
#include "f/nash.h"
/* functions
*/
  struct u2_nash* T_r;

  static u2_noun                                                  //  produce
  _cue_in(u2_wire wir_r,
          u2_atom a,                                              //  retain
          u2_atom b)                                              //  retain
  {
    u2_noun p, q;

    if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, b, 1, a) ) {
      u2_noun x = j2_mbc(Pt1, inc)(wir_r, b);
      u2_noun c = j2_mby(Pt5, rub)(wir_r, x, a);

      p = j2_mbc(Pt1, inc)(wir_r, u2_h(c));
      q = u2_rx(wir_r, u2_t(c));
      u2_na_put(T_r, b, q);

      u2_rz(wir_r, c);
      u2_rz(wir_r, x);
    }
    else {
      u2_noun c = j2_mbc(Pt1, add)(wir_r, _2, b);
      u2_noun l = j2_mbc(Pt1, inc)(wir_r, b);

      if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, l, 1, a) ) {
        u2_noun u, v, w;
        u2_noun x, y;

        u = _cue_in(wir_r, a, c);
        x = j2_mbc(Pt1, add)(wir_r, u2_h(u), c);
        v = _cue_in(wir_r, a, x);

        w = u2_bc(wir_r, u2_rx(wir_r, u2_h(u2_t(u))),
                         u2_rx(wir_r, u2_h(u2_t(v))));

        y = j2_mbc(Pt1, add)(wir_r, u2_h(u), u2_h(v));

        p = j2_mbc(Pt1, add)(wir_r, _2, y);
        q = w;
        u2_na_put(T_r, b, q);

        u2_rz(wir_r, u); u2_rz(wir_r, v); u2_rz(wir_r, x); u2_rz(wir_r, y);
      }
      else {
        u2_noun d = j2_mby(Pt5, rub)(wir_r, c, a);
        u2_noun x = u2_na_get(T_r, u2_t(d));

        p = j2_mbc(Pt1, add)(wir_r, _2, u2_h(d));
        if ( u2_none == x ) {
          return u2_bl_bail(wir_r, c3__exit);
        }
        q = u2_rx(wir_r, x);

        u2_rz(wir_r, d);
      }
      u2_rz(wir_r, l);
      u2_rz(wir_r, c);
    }
    return u2_bt(wir_r, p, q, 0);
  }

  u2_noun                                                         //  transfer
  j2_mby(Pt5, cue)(u2_wire wir_r,
                   u2_atom a)                                     //  retain
  {
    T_r = u2_na_make();

    u2_noun x = _cue_in(wir_r, a, _0);
    u2_noun y = u2_rx(wir_r, u2_h(u2_t(x)));

    u2_na_take(T_r);
    T_r = 0;

    u2_rz(wir_r, x);
    return y;
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
