/* j/2/scag.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qb_scag(u3_atom a,
            u3_noun b)
  {
    if ( !_(u3a_is_cat(a)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      u3_noun acc = u3_nul;
      c3_w i_w = a;

      if ( !i_w )
        return u3_nul;

      while ( i_w ) {
        if ( c3n == u3du(b) ) {
          return u3kb_flop(acc);
        }
        acc = u3nc(u3k(u3h(b)), acc);
        b = u3t(b);
        i_w--;
      }

      return u3kb_flop(acc);
    }
  }

  u3_noun
  u3wb_scag(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_scag(a, b);
    }
  }

