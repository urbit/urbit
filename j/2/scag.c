/* j/2/scag.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_scag(
                    u3_atom a,
                    u3_noun b)
  {
    if ( !_(u3_ca_is_cat(a)) ) {
      return u3_cm_bail(c3__fail);
    }
    else {
      u3_noun acc;
      c3_w i_w = a;

      if ( !i_w )
	return u3_nul;

      while ( i_w ) {
        if ( c3n == u3du(b) ) {
          return u3_nul;
        }
	acc = u3_ci_cell( u3h(b), acc );
	b = u3t(b);
	i_w--;
      }

      return u3_ckb_flop(acc);
    }
  }

  u3_noun
  u3_cwb_scag(
                   u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_scag(a, b);
    }
  }

