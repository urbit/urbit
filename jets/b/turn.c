/* j/2/turn.c
**
*/
#include "all.h"

  static u3_noun
  _turn_in(u3j_site* sit_u, u3_noun a)
  {
    if ( u3_nul == a ) {
      return u3_nul;
    }
    else {
      return u3nc(
          u3j_gate_slam(sit_u, u3k(u3h(a))),
          _turn_in(sit_u, u3t(a)));
    }
  }

/* functions
*/
  u3_noun
  u3qb_turn(u3_noun a, u3_noun b)
  {
    u3_noun  pro;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(b));
    pro = _turn_in(&sit_u, a);
    u3j_gate_lose(&sit_u);
    return pro;
  }

  u3_noun
  u3wb_turn(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_turn(a, b);
    }
  }

