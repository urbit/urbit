/* j/4/by_has.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qdb_has(u3_noun a,
            u3_noun b)
  {
    if ( u3_nul == a ) {
      return c3n;
    }
    else {
      u3_noun l_a, n_a, r_a;
      u3_noun pn_a, qn_a;

      if ( (c3n == u3r_trel(a, &n_a, &l_a, &r_a)) ||
           (c3n == u3r_cell(n_a, &pn_a, &qn_a)) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        if ( (c3y == u3r_sing(b, pn_a)) ) {
          return c3y;
        }
        else {
          if ( c3y == u3qc_gor(b, pn_a) ) {
            return u3qdb_has(l_a, b);
          }
          else return u3qdb_has(r_a, b);
        }
      }
    }
  }
  u3_noun
  u3wdb_has(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdb_has(a, b);
    }
  }
  u3_noun
  u3kdb_has(u3_noun a,
            u3_noun b)
  {
    u3_weak c = u3qdb_has(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3m_bail(c3__exit);
    }
    else return c;
  }

