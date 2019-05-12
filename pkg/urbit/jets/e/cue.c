/* j/5/cue.c
**
*/
#include "all.h"


  static u3_noun
  _cue_in(u3p(u3h_root) har_p,
          u3_atom       a,
          u3_atom       b)
  {
    u3_noun p, q;

    if ( 0 == u3qc_cut(0, b, 1, a) ) {
      u3_noun x = u3qa_inc(b);
      u3_noun c = u3qe_rub(x, a);

      p = u3qa_inc(u3k(u3h(c)));
      q = u3k(u3t(c));

      u3h_put(har_p, u3k(b), u3k(q));

      u3z(c);
      u3z(x);
    }
    else {
      u3_noun c = u3qa_add(2, b);
      u3_noun l = u3qa_inc(b);

      if ( 0 == u3qc_cut(0, l, 1, a) ) {
        u3_noun u, v, w;
        u3_noun x, y;

        u = _cue_in(har_p, a, c);
        x = u3qa_add(u3h(u), c);
        v = _cue_in(har_p, a, x);
        w = u3nc(u3k(u3h(u3t(u))), u3k(u3h(u3t(v))));

        y = u3qa_add(u3h(u), u3h(v));
        p = u3qa_add(2, y);

        q = w;
        u3h_put(har_p, u3k(b), u3k(q));

        u3z(u); u3z(v); u3z(x); u3z(y);
      }
      else {
        u3_noun d = u3qe_rub(c, a);
        u3_noun x = u3h_get(har_p, u3k(u3t(d)));

        p = u3qa_add(2, u3h(d));
        if ( u3_none == x ) {
          return u3m_bail(c3__exit);
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
u3qe_cue(u3_atom a)
{
  u3p(u3h_root) har_p = u3h_new();

  u3_noun x = _cue_in(har_p, a, 0);
  u3_noun y = u3k(u3h(u3t(x)));

  u3h_free(har_p);

  u3z(x);
  return y;
}

u3_noun
u3we_cue(u3_noun cor)
{
  u3_noun a;

  if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
    return u3m_bail(c3__fail);
  } else {
    return u3qe_cue(a);
  }
}

u3_noun
u3ke_cue(u3_atom a)
{
  u3_noun b = u3qe_cue(a);

  u3z(a);
  return b;
}
