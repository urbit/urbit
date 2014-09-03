/* j/3/mat.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  produce
  u2_cqe_mat(
                   u2_atom a)                                     //  retain
  {
    if ( 0 == a ) {
      return u2nc(1, 1);
    } else {
      u2_atom b = u2_cqc_met(0, a);
      u2_atom c = u2_cqc_met(0, b);
      u2_atom u, v, w, x, y, z;
      u2_atom p, q;

      u = u2_cqa_dec(c);
      v = u2_cqa_add(c, c);
      w = u2_cqc_bex(c);
      x = u2_cqc_end(0, u, b);
      y = u2_cqc_lsh(0, u, a);
      z = u2_cqc_mix(x, y);

      p = u2_cqa_add(v, b);
      q = u2_cqc_cat(0, w, z);

      u2z(u);
      u2z(v);
      u2z(w);
      u2z(x);
      u2z(y);
      u2z(z);

      return u2nc(p, q);
    }
  }
  u2_noun                                                         //  transfer
  u2_cwe_mat(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqe_mat(a);
    }
  }
