/* j/2/lien.c
**
*/
#include "all.h"

  static u3_noun
  _lien_in(u3j_site* sit_u, u3_noun a)
  {
    if ( 0 == a ) {
      return c3n;
    } else {
      u3_noun loz;

      if ( c3n == u3du(a) ) {
        return u3m_bail(c3__exit);
      }
      else switch ( (loz = u3j_gate_slam(sit_u, u3k(u3h(a)))) ) {
        case c3y:  return c3y;
        case c3n:  return _lien_in(sit_u, u3t(a));
        default:   u3z(loz);
                   return u3m_bail(c3__exit);
      }
    }
  }

/* functions
*/
  u3_noun
  u3qb_lien(u3_noun a,
            u3_noun b)
  {
    u3_noun  pro;
    u3j_site sit_u;
    u3j_gate_prep(&sit_u, u3k(b));
    pro = _lien_in(&sit_u, a);
    u3j_gate_lose(&sit_u);
    return pro;
  }
  u3_noun
  u3wb_lien(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_lien(a, b);
    }
  }
