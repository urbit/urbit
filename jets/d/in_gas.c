/* j/4/gas.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qdi_gas(u3_noun a,
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
        u3_noun c;

        if ( u3_none == (c = u3qdi_put(a, i_b)) ) {
          return u3m_bail(c3__exit);
        } else {
          u3_noun d = u3qdi_gas(c, t_b);

          u3z(c);
          return d;
        }
      }
    }
  }
  u3_noun
  u3wdi_gas(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_gas(a, b);
    }
  }
  u3_noun
  u3kdi_gas(u3_noun a,
            u3_noun b)
  {
    u3_weak c = u3qdi_gas(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3m_bail(c3__exit);
    }
    else return c;
  }

