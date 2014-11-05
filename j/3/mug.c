/* j/3/mug.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cwc_mug(u3_noun cor)
  {
    u3_noun sam;

    if ( c3nne == (sam = u3_cr_at(u3_cv_sam, cor)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cr_mug(sam);
    }
  }
