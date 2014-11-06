/* j/3/gor.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_gor(u3_noun a,
                   u3_noun b)
  {
    c3_w c_w = u3r_mug(a);
    c3_w d_w = u3r_mug(b);

    if ( c_w == d_w ) {
      return u3_cqc_dor(a, b);
    }
    else return (c_w < d_w) ? c3y : c3n;
  }
  u3_noun
  u3_cwc_gor(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3_cqc_gor(a, b);
    }
  }

