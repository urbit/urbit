/* j/5/cue.c
**
** This file is in the public domain.
*/
#include "all.h"


  static u2_noun
  _cue_in(u2_ch_root* har_u,
          u2_atom     a,
          u2_atom     b)
  {
    u2_noun p, q;

    if ( 0 == u2_cqc_cut(0, b, 1, a) ) {
      u2_noun x = u2_cqa_inc(b);
      u2_noun c = u2_cqe_rub(x, a);

      p = u2_cqa_inc(u2k(u2h(c)));
      q = u2k(u2t(c));

      u2_ch_put(har_u, u2k(b), u2k(q));

      u2z(c);
      u2z(x);
    }
    else {
      u2_noun c = u2_cqa_add(2, b);
      u2_noun l = u2_cqa_inc(b);

      if ( 0 == u2_cqc_cut(0, l, 1, a) ) {
        u2_noun u, v, w;
        u2_noun x, y;

        u = _cue_in(har_u, a, c);
        x = u2_cqa_add(u2h(u), c);
        v = _cue_in(har_u, a, x);
        w = u2nc(u2k(u2h(u2t(u))), u2k(u2h(u2t(v))));

        y = u2_cqa_add(u2h(u), u2h(v));
        p = u2_cqa_add(2, y);

        q = w;
        u2_ch_put(har_u, u2k(b), u2k(q));

        u2z(u); u2z(v); u2z(x); u2z(y);
      }
      else {
        u2_noun d = u2_cqe_rub(c, a);
        u2_noun x = u2_ch_get(har_u, u2k(u2t(d)));

        p = u2_cqa_add(2, u2h(d));
        if ( u2_none == x ) {
          return u2_cm_bail(c3__exit);
        }
        q = x;
        u2z(d);
      }
      u2z(l);
      u2z(c);
    }
    return u2nt(p, q, 0);
  }

  u2_noun
  u2_cqe_cue(u2_atom a)
  {
    u2_ch_root* har_u = u2_ch_new();

    u2_noun x = _cue_in(har_u, a, 0);
    u2_noun y = u2k(u2h(u2t(x)));

    u2_ch_free(har_u);

    u2z(x);
    return y;
  }
  u2_noun
  u2_cwe_cue(u2_noun cor)
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqe_cue(a);
    }
  }
  u2_noun
  u2_cke_cue(u2_atom a)
  {
    u2_noun b = u2_cqe_cue(a);

    u2z(a);
    return b;
  }

