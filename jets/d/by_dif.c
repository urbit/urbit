/* jets/d/by_dif.c
**
*/
#include "all.h"

/* internal functions
*/
  u3_noun _b_dif_join(u3_noun d,
                      u3_noun e)
  {
    if ( u3_nul == d ) {
      return u3k(e);
    } else if (u3_nul == e) {
      return u3k(d);
    } else {
      u3_noun n_d, l_d, r_d;
      u3_noun n_e, l_e, r_e;
      u3_noun p_n_d, q_n_d;
      u3_noun p_n_e, q_n_e;

      if (    c3n == u3r_trel(d, &n_d, &l_d, &r_d)
           || c3n == u3r_trel(e, &n_e, &l_e, &r_e)
           || c3n == u3r_cell(n_d, &p_n_d, &q_n_d)
           || c3n == u3r_cell(n_e, &p_n_e, &q_n_e) ) {
        return u3m_bail(c3__exit);
      } else {
        if ( c3y == u3qc_vor(p_n_d, p_n_e) ) {
          return u3nt(u3k(n_d),
                      u3k(l_d),
                      _b_dif_join(u3k(r_d), u3k(e)));
        } else {
          return u3nt(u3k(n_e),
                      _b_dif_join(u3k(d), u3k(l_e)),
                      u3k(r_e));
        }
      }
    }
  }

/* functions
*/
  u3_noun u3wdb_dif(u3_noun cor)
  {
    u3_noun a, b;
    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdb_dif(a, b);
    }
  }

  u3_noun u3qdb_dif(u3_noun a,
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

        c = u3qdb_bif(a, n_b);

        if ( c3n == u3r_cell(c, &l_c, &r_c) ) {
          return u3m_bail(c3__exit);
        } else {
          u3_noun d;
          u3_noun e;

          d = u3qdb_dif(l_c, l_b);
          e = u3qdb_dif(r_c, r_b);
          u3z(c);

          return _b_dif_join(d, e);
        }
      }
    }
  }

  /*
  u3_weak u3kdb_dif(u3_noun a, u3_noun b);
  */
