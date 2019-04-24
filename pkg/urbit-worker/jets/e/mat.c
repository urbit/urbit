/* j/3/mat.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qe_mat(u3_atom a)
  {
    if ( 0 == a ) {
      return u3nc(1, 1);
    } else {
      u3_atom b = u3qc_met(0, a);
      u3_atom c = u3qc_met(0, b);
      u3_atom u, v, w, x, y, z;
      u3_atom p, q;

      u = u3qa_dec(c);
      v = u3qa_add(c, c);
      w = u3qc_bex(c);
      x = u3qc_end(0, u, b);
      y = u3qc_lsh(0, u, a);
      z = u3qc_mix(x, y);

      p = u3qa_add(v, b);
      q = u3qc_cat(0, w, z);

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
  u3we_mat(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_mat(a);
    }
  }
