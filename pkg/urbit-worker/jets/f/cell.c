/* j/6/cell.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_cell(u3_noun hed,
            u3_noun tal)
  {
    if ( (c3__void == hed) || (c3__void == tal) ) {
      return c3__void;
    } else {
      return u3nt(c3__cell, u3k(hed), u3k(tal));
    }
  }
  u3_noun
  u3wf_cell(u3_noun cor)
  {
    u3_noun hed, tal;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &hed, u3x_sam_3, &tal, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_cell(hed, tal);
    }
  }
