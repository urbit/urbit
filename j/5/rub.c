/* j/3/rub.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqe_rub(
                   u2_atom a,
                   u2_atom b)
  {
    u2_atom c, d, e;
    u2_atom w, x, y, z;
    u2_atom p, q;

    u2_atom m = u2_cqa_add(a, u2_cr_met(0, b));

    //  Compute c and d.
    {
      x = u2k(a);

      while ( 0 == u2_cqc_cut(0, x, 1, b) ) {
        u2_atom y = u2_cqa_inc(x);

        //  Sanity check: crash if decoding more bits than available
        if ( u2_yes == u2_cqa_gth(x, m)) {
          //  fprintf(stderr, "[%%rub-hard %d %d %d]\r\n", a, x, m);
          return u2_cm_bail(c3__exit);
        }

        u2z(x);
        x = y;
      }
      if ( u2_yes == u2_cr_sing(x, a) ) {
        u2z(x);
        return u2nc(1, 0);
      }
      c = u2_cqa_sub(x, a);
      d = u2_cqa_inc(x);

      u2z(x);
    }

    //  Compute e, p, q.
    {
      x = u2_cqa_dec(c);
      y = u2_cqc_bex(x);
      z = u2_cqc_cut(0, d, x, b);

      e = u2_cqa_add(y, z);
      u2z(y); u2z(z);

      w = u2_cqa_add(c, c);
      y = u2_cqa_add(w, e);
      z = u2_cqa_add(d, x);

      p = u2_cqa_add(w, e);
      q = u2_cqc_cut(0, z, e, b);

      u2z(w); u2z(x); u2z(y); u2z(z);

      return u2nc(p, q);
    }
  }
  u2_noun
  u2_cwe_rub(
                  u2_noun cor)
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqe_rub(a, b);
    }
  }
