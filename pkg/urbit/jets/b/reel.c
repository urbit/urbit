/* j/2/reel.c
**
*/
#include "all.h"

/* functions
*/
  u3_noun
  u3qb_reel(u3_noun a,
            u3_noun b)
  {
    u3_noun pro = u3k(u3x_at(u3x_sam_3, b));
    if ( u3_nul == a ) {
      return pro;
    }
    else {
      u3j_site sit_u;
      u3_noun  i, t = a;
      c3_w     j_w, len_w = 0, all_w = 89, pre_w = 55;
      u3_noun* vec = u3a_malloc(all_w);

      // stuff list into an array
      do {
        if ( c3n == u3r_cell(t, &i, &t) ) {
          u3a_free(vec);
          return u3m_bail(c3__exit);
        }
        else {
          if ( len_w == all_w ) {
            // grow vec fib-wise
            all_w += pre_w;
            pre_w = len_w;
            vec = u3a_realloc(vec, all_w);
          }
          vec[len_w++] = i;
        }
      } while ( u3_nul != t );

      // now we can iterate backwards
      u3j_gate_prep(&sit_u, u3k(b));
      for ( j_w = len_w; j_w > 0; ) {
        pro = u3j_gate_slam(&sit_u, u3nc(u3k(vec[--j_w]), pro));
      }
      u3j_gate_lose(&sit_u);
      u3a_free(vec);
      return pro;
    }
  }
  u3_noun
  u3wb_reel(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_reel(a, b);
    }
  }
