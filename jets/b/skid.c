/* j/2/skid.c
**
*/
#include "all.h"

  static u3_noun
  _skid_in(u3j_site* sit_u, u3_noun a)
  {
    if ( 0 == a ) {
      return u3nc(u3_nul, u3_nul);
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    } else {
      u3_noun acc = _skid_in(sit_u, u3t(a));
      u3_noun hoz = u3j_gate_slam(sit_u, u3k(u3h(a)));
      u3_noun nex;

      if ( c3y == hoz ) {
        nex = u3nc(u3nc(u3k(u3h(a)), u3k(u3h(acc))), u3k(u3t(acc)));
      }
      else {
        nex = u3nc(u3k(u3h(acc)), u3nc(u3k(u3h(a)), u3k(u3t(acc))));
      }
      u3z(hoz);
      u3z(acc);

      return nex;
    }
  }

/* functions
*/
  u3_noun
  u3qb_skid(u3_noun a,
            u3_noun b)
  {
    u3_noun  pro;
    u3j_site sit_u;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _skid_in(&sit_u, a);
    u3j_gate_lose(&sit_u);
    return pro;
  }
  u3_noun
  u3wb_skid(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_skid(a, b);
    }
  }
