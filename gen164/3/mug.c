/* j/3/mug.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cwc_mug(
                  u2_noun cor)
  {
    u2_noun sam;

    if ( u2_none == (sam = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cr_mug(sam);
    }
  }
