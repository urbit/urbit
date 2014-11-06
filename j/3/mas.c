/* j/3/mas.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_mas(u3_atom a)
  {
    c3_w b_w;
    u3_atom c, d, e, f;

    b_w = u3r_met(0, a);
    if ( b_w < 2 ) {
      return u3m_bail(c3__exit);
    }
    else {
      c = u3_cqc_bex((b_w - 1));
      d = u3_cqc_bex((b_w - 2));
      e = u3_cqa_sub(a, c);
      f = u3_cqc_con(e, d);

      u3z(c);
      u3z(d);
      u3z(e);

      return f;
    }
  }
  u3_noun
  u3_cwc_mas(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3v_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3_cqc_mas(a);
    }
  }

