/* j/3/vor.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3gc_vor(u3_atom a,
           u3_atom b)
  {
    c3_w c_w = u3r_mug(u3r_mug(a));
    c3_w d_w = u3r_mug(u3r_mug(b));

    if ( c_w == d_w ) {
      return u3gc_dor(a, b);
    }
    else return (c_w < d_w) ? c3y : c3n;
  }
  u3_noun
  u3yc_vor(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3gc_vor(a, b);
    }
  }
