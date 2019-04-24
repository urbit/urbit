/* j/4/in_tap.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _tap_in(u3_noun a,
          u3_noun b)
  {
    if ( u3_nul == a ) {
      return b;
    } else {
      u3_noun l_a, n_a, r_a;

      if ( (c3n == u3r_trel(a, &n_a, &l_a, &r_a)) ) {
        u3z(b);
        return u3m_bail(c3__exit);
      } else {
        return _tap_in(r_a,
                       u3nc(u3k(n_a),
                       _tap_in(l_a, b)));
      }
    }
  }

  u3_noun
  u3qdi_tap(u3_noun a)
  {
    return _tap_in(a, u3_nul);
  }
  u3_noun
  u3wdi_tap(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == u3r_mean(cor, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_tap(a);
    }
  }
  u3_noun
  u3kdi_tap(u3_noun a)
  {
    u3_weak b = u3qdi_tap(a);

    u3z(a);
    if ( u3_none == b ) {
      return u3m_bail(c3__exit);
    }
    else return b;
  }
