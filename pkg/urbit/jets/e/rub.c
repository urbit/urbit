/* j/3/rub.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qe_rub(u3_atom a,
           u3_atom b)
  {
    u3_atom c, d, e;
    u3_atom w, x, y, z;
    u3_atom p, q;

    u3_atom m = u3qa_add(a, u3r_met(0, b));

    //  Compute c and d.
    {
      x = u3k(a);

      while ( 0 == u3qc_cut(0, x, 1, b) ) {
        u3_atom y = u3qa_inc(x);

        //  Sanity check: crash if decoding more bits than available
        if ( c3y == u3qa_gth(x, m)) {
          //  fprintf(stderr, "[%%rub-hard %d %d %d]\r\n", a, x, m);
          return u3m_bail(c3__exit);
        }

        u3z(x);
        x = y;
      }
      if ( c3y == u3r_sing(x, a) ) {
        u3z(x);
        return u3nc(1, 0);
      }
      c = u3qa_sub(x, a);
      d = u3qa_inc(x);

      u3z(x);
    }

    //  Compute e, p, q.
    {
      x = u3qa_dec(c);
      y = u3qc_bex(x);
      z = u3qc_cut(0, d, x, b);

      e = u3qa_add(y, z);
      u3z(y); u3z(z);

      w = u3qa_add(c, c);
      y = u3qa_add(w, e);
      z = u3qa_add(d, x);

      p = u3qa_add(w, e);
      q = u3qc_cut(0, z, e, b);

      u3z(w); u3z(x); u3z(y); u3z(z);

      return u3nc(p, q);
    }
  }
  u3_noun
  u3we_rub(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_rub(a, b);
    }
  }
