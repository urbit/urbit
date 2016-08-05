/* j/2/slag.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_slag(u3_atom a, u3_noun b)
  {
    if ( u3_nul == b ) {
      return u3_nul;
    }
    else if ( !_(u3a_is_cat(a)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      c3_w len_w = a;

      while ( len_w ) {
        if ( c3n == u3du(b) ) {
          return u3_nul;
        }
        b = u3t(b);
        len_w--;
      }
      return u3k(b);
    }
  }
  u3_noun
  u3wb_slag(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a) && u3_nul != b) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_slag(a, b);
    }
  }
