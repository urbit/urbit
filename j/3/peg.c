/* j/3/peg.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_peg(
                   u3_atom a,
                   u3_atom b)
  {
    u3_atom c, d, e, f, g, h;

    c = u3_cr_met(0, b);
    d = u3_cqa_dec(c);
    e = u3_cqc_lsh(0, d, 1);
    f = u3_cqa_sub(b, e);
    g = u3_cqc_lsh(0, d, a);
    h = u3_cqa_add(f, g);

    u3z(c);
    u3z(d);
    u3z(e);
    u3z(f);
    u3z(g);

    return h;
  }
  u3_noun
  u3_cwc_peg(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (0 == a) ||
         (0 == b) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_peg(a, b);
    }
  }

