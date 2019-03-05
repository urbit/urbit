/* j/2/skip.c
**
*/
#include "all.h"

  static u3_noun
  _skip_in(u3j_site* sit_u, u3_noun a)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3_none;
    } else {
      u3_noun hoz = u3j_gate_slam(sit_u, u3k(u3h(a)));
      u3_noun vyr = _skip_in(sit_u, u3t(a));

      switch ( hoz ) {
        case c3y:  return vyr;
        case c3n:  return u3nc(u3k(u3h(a)), vyr);
        default:   u3z(hoz);
                   u3z(vyr);
                   return u3_none;
      }
    }
  }

/* functions
*/
  u3_noun
  u3qb_skip(u3_noun a,
            u3_noun b)
  {
    u3j_site sit_u;
    u3_noun  pro;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _skip_in(&sit_u, a);
    u3j_gate_lose(&sit_u);
    return pro;
  }
  u3_noun
  u3wb_skip(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3_none;
    } else {
      return u3qb_skip(a, b);
    }
  }

