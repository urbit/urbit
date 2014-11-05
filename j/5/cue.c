/* j/5/cue.c
**
** This file is in the public domain.
*/
#include "all.h"


  static u3_noun
  _cue_in(u3p(u3_ch_root) har_p,
          u3_atom         a,
          u3_atom         b)
  {
    u3_noun p, q;

    if ( 0 == u3_cqc_cut(0, b, 1, a) ) {
      u3_noun x = u3_cqa_inc(b);
      u3_noun c = u3_cqe_rub(x, a);

      p = u3_cqa_inc(u3k(u3h(c)));
      q = u3k(u3t(c));

      u3_ch_put(har_p, u3k(b), u3k(q));

      u3z(c);
      u3z(x);
    }
    else {
      u3_noun c = u3_cqa_add(2, b);
      u3_noun l = u3_cqa_inc(b);

      if ( 0 == u3_cqc_cut(0, l, 1, a) ) {
        u3_noun u, v, w;
        u3_noun x, y;

        u = _cue_in(har_p, a, c);
        x = u3_cqa_add(u3h(u), c);
        v = _cue_in(har_p, a, x);
        w = u3nc(u3k(u3h(u3t(u))), u3k(u3h(u3t(v))));

        y = u3_cqa_add(u3h(u), u3h(v));
        p = u3_cqa_add(2, y);

        q = w;
        u3_ch_put(har_p, u3k(b), u3k(q));

        u3z(u); u3z(v); u3z(x); u3z(y);
      }
      else {
        u3_noun d = u3_cqe_rub(c, a);
        u3_noun x = u3_ch_get(har_p, u3k(u3t(d)));

        p = u3_cqa_add(2, u3h(d));
        if ( c3nne == x ) {
          return u3_cm_bail(c3__exit);
        }
        q = x;
        u3z(d);
      }
      u3z(l);
      u3z(c);
    }
    return u3nt(p, q, 0);
  }

  u3_noun
  u3_cqe_cue(u3_atom a)
  {
    u3p(u3_ch_root) har_p = u3_ch_new();

    u3_noun x = _cue_in(har_p, a, 0);
    u3_noun y = u3k(u3h(u3t(x)));

    u3_ch_free(har_p);

    u3z(x);
    return y;
  }
  u3_noun
  u3_cwe_cue(u3_noun cor)
  {
    u3_noun a;

    if ( (c3nne == (a = u3_cr_at(u3_cv_sam, cor))) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_cue(a);
    }
  }
  u3_noun
  u3_cke_cue(u3_atom a)
  {
    u3_noun b = u3_cqe_cue(a);

    u3z(a);
    return b;
  }

