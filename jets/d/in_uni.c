/* j/4/in_uni.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun 
  u3kdi_uni(u3_noun a,
            u3_noun b)
  {
    u3_noun c = u3qdi_uni(a, b);

    u3z(a);
    u3z(b);
    return c;
  }

  u3_noun
  u3qdi_uni(u3_noun a,
            u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3k(b);
    }
    else if ( u3_nul == b ) {
      return u3k(a);
    }
    else {
      u3_noun l_a, n_a, r_a, lr_a;
      u3_noun l_b, n_b, r_b, lr_b;

      if ( (c3n == u3r_cell(a, &n_a, &lr_a)) ) {
        return u3m_bail(c3__exit);
      }
      else if ( (c3n == u3r_cell(b, &n_b, &lr_b)) ) {
        return u3m_bail(c3__exit);
      }
      else {
        if ( c3y == u3qc_vor(n_a, n_b) ) {
          if ( c3n == u3r_cell(lr_a, &l_a, &r_a) ) {
            return u3m_bail(c3__exit);
          }
          else if ( c3n == u3r_cell(lr_b, &l_b, &r_b) ) {
            return u3m_bail(c3__exit);
          }
          else if ( c3y == u3r_sing(n_a, n_b) ) {
            return u3nt(u3k(n_b),
                        u3kdi_uni(u3k(l_a), u3k(l_b)),
                        u3kdi_uni(u3k(r_a), u3k(r_b)));
          }
          else if ( c3y == u3qc_hor(n_b, n_a) ) {
            return u3kdi_uni(u3nt(u3k(n_a),
                                  u3kdi_uni(u3k(l_a),
                                            u3nt(u3k(n_b),
                                                 u3k(l_b),
                                                 u3_nul)),
                                  u3k(r_a)),
                             u3k(r_b));
          }
          else {
            return u3kdi_uni(u3nt(u3k(n_a),
                                  u3k(l_a),
                                  u3kdi_uni(u3k(r_a),
                                            u3nt(u3k(n_b),
                                                 u3_nul,
                                                 u3k(r_b)))),
                             u3k(l_b));
          }
        }
        else {
          if ( c3n == u3r_cell(lr_b, &l_b, &r_b) ) {
            return u3m_bail(c3__exit);
          }
          else if ( c3n == u3r_cell(lr_a, &l_a, &r_a) ) {
            return u3m_bail(c3__exit);
          }
          else if ( c3y == u3r_sing(n_b, n_a) ) {
            return u3nt(u3k(n_b),
                        u3kdi_uni(u3k(l_a), u3k(l_b)),
                        u3kdi_uni(u3k(r_a), u3k(r_b)));
          }
          else if ( c3y == u3qc_hor(n_a, n_b) ) {
            return u3kdi_uni(u3k(r_a),
                             u3nt(u3k(n_b),
                                  u3kdi_uni(u3nt(u3k(n_a),
                                                 u3k(l_a),
                                                 u3_nul),
                                            u3k(l_b)),
                                  u3k(r_b)));
          }
          else {
            return u3kdi_uni(u3k(l_a),
                             u3nt(u3k(n_b),
                                  u3k(l_b),
                                  u3kdi_uni(u3nt(u3k(n_a),
                                                 u3_nul,
                                                 u3k(r_a)),
                                            u3k(r_b))));
          }
        }
      }
    }
  }
  u3_noun
  u3wdi_uni(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_uni(a, b);
    }
  }

