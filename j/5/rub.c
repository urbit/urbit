/* j/3/rub.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqe_rub(
                   u3_atom a,
                   u3_atom b)
  {
    u3_atom c, d, e;
    u3_atom w, x, y, z;
    u3_atom p, q;

    u3_atom m = u3_cqa_add(a, u3_cr_met(0, b));

    //  Compute c and d.
    {
      x = u3k(a);

      while ( 0 == u3_cqc_cut(0, x, 1, b) ) {
        u3_atom y = u3_cqa_inc(x);

        //  Sanity check: crash if decoding more bits than available
        if ( c3y == u3_cqa_gth(x, m)) {
          //  fprintf(stderr, "[%%rub-hard %d %d %d]\r\n", a, x, m);
          return u3_cm_bail(c3__exit);
        }

        u3z(x);
        x = y;
      }
      if ( c3y == u3_cr_sing(x, a) ) {
        u3z(x);
        return u3nc(1, 0);
      }
      c = u3_cqa_sub(x, a);
      d = u3_cqa_inc(x);

      u3z(x);
    }

    //  Compute e, p, q.
    {
      x = u3_cqa_dec(c);
      y = u3_cqc_bex(x);
      z = u3_cqc_cut(0, d, x, b);

      e = u3_cqa_add(y, z);
      u3z(y); u3z(z);

      w = u3_cqa_add(c, c);
      y = u3_cqa_add(w, e);
      z = u3_cqa_add(d, x);

      p = u3_cqa_add(w, e);
      q = u3_cqc_cut(0, z, e, b);

      u3z(w); u3z(x); u3z(y); u3z(z);

      return u3nc(p, q);
    }
  }
  u3_noun
  u3_cwe_rub(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_rub(a, b);
    }
  }
