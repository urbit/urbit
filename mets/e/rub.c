/* j/3/rub.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3ge_rub(u3_atom a,
           u3_atom b)
  {
    u3_atom c, d, e;
    u3_atom w, x, y, z;
    u3_atom p, q;

    u3_atom m = u3ga_add(a, u3r_met(0, b));

    //  Compute c and d.
    {
      x = u3k(a);

      while ( 0 == u3gc_cut(0, x, 1, b) ) {
        u3_atom y = u3ga_inc(x);

        //  Sanity check: crash if decoding more bits than available
        if ( c3y == u3ga_gth(x, m)) {
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
      c = u3ga_sub(x, a);
      d = u3ga_inc(x);

      u3z(x);
    }

    //  Compute e, p, q.
    {
      x = u3ga_dec(c);
      y = u3gc_bex(x);
      z = u3gc_cut(0, d, x, b);

      e = u3ga_add(y, z);
      u3z(y); u3z(z);

      w = u3ga_add(c, c);
      y = u3ga_add(w, e);
      z = u3ga_add(d, x);

      p = u3ga_add(w, e);
      q = u3gc_cut(0, z, e, b);

      u3z(w); u3z(x); u3z(y); u3z(z);

      return u3nc(p, q);
    }
  }
  u3_noun
  u3ye_rub(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3ge_rub(a, b);
    }
  }
