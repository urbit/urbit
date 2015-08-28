/* j/2/sort.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_sort(u3_noun a,
            u3_noun b)
  {
    //  must think about
    //
    return u3m_bail(c3__fail);
  }
  u3_noun
  u3wb_sort(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_sort(a, b);
    }
  }

