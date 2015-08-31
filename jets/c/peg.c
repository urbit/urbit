/* j/3/peg.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_peg(u3_atom a,
           u3_atom b)
  {
    u3_atom c, d, e, f, g, h;

    c = u3r_met(0, b);
    d = u3qa_dec(c);
    e = u3qc_lsh(0, d, 1);
    f = u3qa_sub(b, e);
    g = u3qc_lsh(0, d, a);
    h = u3qa_add(f, g);

    u3z(c);
    u3z(d);
    u3z(e);
    u3z(f);
    u3z(g);

    return h;
  }
  u3_noun
  u3wc_peg(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (0 == a) ||
         (0 == b) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_peg(a, b);
    }
  }

