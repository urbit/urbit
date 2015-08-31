/* j/4/gas.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qdb_gas(u3_noun a,
            u3_noun b)
  {
    if ( u3_nul == b ) {
      return u3k(a);
    }
    else {
      if ( c3n == u3du(b) ) {
        return u3m_bail(c3__exit);
      } else {
        u3_noun i_b = u3h(b);
        u3_noun t_b = u3t(b);

        if ( c3n == u3du(i_b) ) {
          return u3m_bail(c3__exit);
        } else {
          u3_noun pi_b = u3h(i_b);
          u3_noun qi_b = u3t(i_b);
          u3_noun c;

          if ( u3_none == (c = u3qdb_put(a, pi_b, qi_b)) ) {
            return u3m_bail(c3__exit);
          } else {
            u3_noun d = u3qdb_gas(c, t_b);

            u3z(c);
            return d;
          }
        }
      }
    }
  }
  u3_noun
  u3wdb_gas(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdb_gas(a, b);
    }
  }
  u3_noun
  u3kdb_gas(u3_noun a,
            u3_noun b)
  {
    u3_weak c = u3qdb_gas(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3m_bail(c3__exit);
    }
    else return c;
  }

