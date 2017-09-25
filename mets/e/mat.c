/* j/3/mat.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3ge_mat(u3_atom a)
  {
    if ( 0 == a ) {
      return u3nc(1, 1);
    } else {
      u3_atom b = u3gc_met(0, a);
      u3_atom c = u3gc_met(0, b);
      u3_atom u, v, w, x, y, z;
      u3_atom p, q;

      u = u3ga_dec(c);
      v = u3ga_add(c, c);
      w = u3gc_bex(c);
      x = u3gc_end(0, u, b);
      y = u3gc_lsh(0, u, a);
      z = u3gc_mix(x, y);

      p = u3ga_add(v, b);
      q = u3gc_cat(0, w, z);

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
  u3ye_mat(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3ge_mat(a);
    }
  }
