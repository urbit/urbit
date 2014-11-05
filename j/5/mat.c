/* j/3/mat.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqe_mat(
                   u3_atom a)
  {
    if ( 0 == a ) {
      return u3nc(1, 1);
    } else {
      u3_atom b = u3_cqc_met(0, a);
      u3_atom c = u3_cqc_met(0, b);
      u3_atom u, v, w, x, y, z;
      u3_atom p, q;

      u = u3_cqa_dec(c);
      v = u3_cqa_add(c, c);
      w = u3_cqc_bex(c);
      x = u3_cqc_end(0, u, b);
      y = u3_cqc_lsh(0, u, a);
      z = u3_cqc_mix(x, y);

      p = u3_cqa_add(v, b);
      q = u3_cqc_cat(0, w, z);

      u3z(u);
      u3z(v);
      u3z(w);
      u3z(x);
      u3z(y);
      u3z(z);

      return u3nc(p, q);
    }
  }
  u3_noun
  u3_cwe_mat(
                  u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam, cor))) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_mat(a);
    }
  }
