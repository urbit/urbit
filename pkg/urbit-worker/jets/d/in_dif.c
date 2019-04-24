/* jets/d/in_dif.c
**
*/
#include "all.h"

/* internal functions
*/
  static
  u3_noun _i_dif_join(u3_noun d,
                      u3_noun e)
  {
    if ( u3_nul == d ) {
      return u3k(e);
    } else if (u3_nul == e) {
      return u3k(d);
    } else {
      u3_noun n_d, l_d, r_d;
      u3_noun n_e, l_e, r_e;

      if (    c3n == u3r_trel(d, &n_d, &l_d, &r_d)
           || c3n == u3r_trel(e, &n_e, &l_e, &r_e) ) {
        return u3m_bail(c3__exit);
      } else {
        if ( c3y == u3qc_mor(n_d, n_e) ) {
          return u3nt(u3k(n_d),
                      u3k(l_d),
                      _i_dif_join(u3k(r_d), u3k(e)));
        } else {
          return u3nt(u3k(n_e),
                      _i_dif_join(u3k(d), u3k(l_e)),
                      u3k(r_e));
        }
      }
    }
  }

/* functions
*/
  u3_noun u3wdi_dif(u3_noun cor)
  {
    u3_noun a, b;
    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_dif(a, b);
    }
  }

  u3_noun u3qdi_dif(u3_noun a,
                    u3_noun b)
  {
    if ( u3_nul == b ) {
      return u3k(a);
    } else {
      u3_noun n_b, l_b, r_b;
      u3_noun c, l_c, r_c;

      if ( c3n == u3r_trel(b, &n_b, &l_b, &r_b) ) {
        return u3m_bail(c3__exit);
      } else {

        c = u3qdi_bif(a, n_b);

        if ( c3n == u3r_cell(c, &l_c, &r_c) ) {
          return u3m_bail(c3__exit);
        } else {
          u3_noun d;
          u3_noun e;

          d = u3qdi_dif(l_c, l_b);
          e = u3qdi_dif(r_c, r_b);
          u3z(c);

          return _i_dif_join(d, e);
        }
      }
    }
  }

  /*
  u3_weak u3kdi_dif(u3_noun a, u3_noun b);
  */
