/* j/4/in_has.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qdi_has(u3_noun a,
            u3_noun b)
  {
    if ( u3_nul == a ) {
      return c3n;
    }
    else {
      u3_noun l_a, n_a, r_a;

      if ( (c3n == u3r_mean(a, 2, &n_a, 6, &l_a, 7, &r_a, 0)) ) {
        return u3m_bail(c3__exit);
      }
      else {
        if ( (c3y == u3r_sing(b, n_a)) ) {
          return c3y;
        }
        else {
          if ( c3y == u3qc_hor(b, n_a) ) {
            return u3qdi_has(l_a, b);
          }
          else return u3qdi_has(r_a, b);
        }
      }
    }
  }
  u3_noun
  u3wdi_has(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_has(a, b);
    }
  }
  u3_noun
  u3kdi_has(u3_noun a,
            u3_noun b)
  {
    u3_weak c = u3qdi_has(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3m_bail(c3__exit);
    }
    else return c;
  }

