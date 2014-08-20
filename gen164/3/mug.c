/* j/3/mug.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mb(Pt3, mug)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun sam;

    if ( u2_none == (sam = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cr_mug(sam);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, mug)[] = {
    { ".2", c3__lite, j2_mb(Pt3, mug), Tier3, u2_none, u2_none },
    { }
  };
