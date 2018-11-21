/* j/2/roll.c
**
*/
#include "all.h"

  static u3_noun
  _roll_in(u3j_site* sit_u, u3_noun a, u3_noun b)
  {
    if ( 0 == a ) {
      return b;
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      b = u3j_gate_slam(sit_u, u3nc(u3k(u3h(a)), b));
      return _roll_in(sit_u, u3t(a), b);
    }
  }

/* functions
*/
  u3_noun
  u3qb_roll(u3_noun a,
            u3_noun b)
  {
    u3_noun  pro;
    u3j_site sit_u;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _roll_in(&sit_u, a, u3k(u3x_at(u3x_sam_3, b)));
    u3j_gate_lose(&sit_u);
    return pro;
  }
  u3_noun
  u3wb_roll(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_roll(a, b);
    }
  }

