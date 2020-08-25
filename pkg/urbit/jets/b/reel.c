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
    if ( u3_nul != a ) {
      u3j_site sit_u;
      u3_noun* top;
      u3_noun  i, t = a;
      c3_w     len_w = 0;

      // push list onto road stack
      do {
        u3x_cell(t, &i, &t);
        top = (c3_w*) u3a_push(sizeof(u3_noun));
        *top = i;
        ++len_w;
      } while ( u3_nul != t );

      u3j_gate_prep(&sit_u, u3k(b));
      while ( len_w-- > 0 ) {
        top = u3a_peek(sizeof(u3_noun));
        u3a_pop(sizeof(u3_noun));
        pro = u3j_gate_slam(&sit_u, u3nc(u3k(*top), pro));
      }
      u3j_gate_lose(&sit_u);
    }
    return pro;
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
