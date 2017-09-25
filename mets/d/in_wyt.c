/* jets/d/in_wyt.c
**
*/
#include "all.h"

/* functions
*/
  u3_noun u3ydi_wyt(u3_noun cor)
  {
    u3_noun a;
    if ( c3n == u3r_mean(cor, u3x_con_2, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3gdi_wyt(a);
    }
  }

  u3_noun u3gdi_wyt(u3_noun a)
  {
    u3_noun n_a, l_a, r_a;
    if ( u3_nul == a ) {
      return 0;
    }
    else if ( c3n == u3r_trel(a, &n_a, &l_a, &r_a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3i_vint(u3la_add(u3gdi_wyt(l_a), u3gdi_wyt(r_a)));
    }
  }
