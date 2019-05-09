/* j/2/murn.c
**
*/
#include "all.h"

  u3_noun
  _murn_in(u3j_site* sit_u, u3_noun a)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun one = u3j_gate_slam(sit_u, u3k(u3h(a)));
      u3_noun two = _murn_in(sit_u, u3t(a));
      u3_noun nex;

      switch ( u3ud(one) ) {
        case c3y:  u3z(one);
                   return two;
        case c3n:  nex = u3nc(u3k(u3t(one)), two);
                   u3z(one);
                   return nex;
        default:   u3z(one);
                   u3z(two);
                   return u3_none;
      }
    }
  }

/* functions
*/
  u3_noun
  u3qb_murn(u3_noun a, u3_noun b)
  {
    u3_noun pro;
    u3j_site sit_u;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _murn_in(&sit_u, a);
    u3j_gate_lose(&sit_u);
    return pro;
  }
  u3_noun
  u3wb_murn(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_murn(a, b);
    }
  }

