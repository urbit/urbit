/* j/2/reel.c
**
*/
#include "all.h"

  static u3_noun
  _reel_in(u3j_site* sit_u, u3_noun a, u3_noun b)
  {
    if ( 0 == a ) {
      return b;
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun gim = u3k(u3h(a));
      u3_noun hur = _reel_in(sit_u, u3t(a), b);

      return u3j_gate_slam(sit_u, u3nc(gim, hur));
    }
  }

/* functions
*/
  u3_noun
  u3qb_reel(u3_noun a,
            u3_noun b)
  {
    u3_noun  pro;
    u3j_site sit_u;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _reel_in(&sit_u, a, u3k(u3x_at(u3x_sam_3, b)));
    u3j_gate_lose(&sit_u);
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
