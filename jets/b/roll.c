/* j/2/roll.c
**
*/
#include "all.h"

  static u3_noun
  _roll_in(u3j_site* sit_u, u3_noun a, u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(b);
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun gim = u3k(u3h(a));
      u3_noun zor = u3k(b);
      u3_noun daz = u3j_gate_slam(sit_u, u3nc(gim, zor));
      u3_noun hox = _roll_in(sit_u, u3t(a), daz);
      u3z(daz);
      return hox;
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
    u3j_gate_prep(&sit_u, b);
    pro = _roll_in(&sit_u, a, u3r_at(u3x_sam_3, b));
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

