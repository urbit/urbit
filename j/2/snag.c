/* j/2/snag.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_snag(u3_atom a,
                    u3_noun b)
  {
    if ( u3_ne(u3_ca_is_cat(a)) ) {
      return u3_cm_bail(c3__fail);
    }
    else {
      c3_w len_w = a;

      while ( len_w ) {
        if ( u3_no == u3du(b) ) {
          return u3_cm_bail(c3__exit);
        }
        b = u3t(b);
        len_w--;
      }
      if ( u3_no == u3du(b) ) {
        return u3_cm_bail(c3__exit);
      }
      return u3k(u3h(b));
    }
  }
  u3_noun
  u3_cwb_snag(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_snag(a, b);
    }
  }
