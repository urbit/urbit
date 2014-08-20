/* j/6/flip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, flip)(
                    u2_noun hel)                                  //  retain
  {
    if ( u2_yes == u2_cr_sing(1, u2h(hel)) ) {
      if ( (u2_yes == u2t(hel)) ) {
        return u2nc(1, u2_no);
      }
      else {
        c3_assert((u2_no == u2t(hel)));

        return u2nc(1, u2_yes);
      }
    }
    else {
      return u2nq
        (6,
               u2k(hel),
               u2nc(1, u2_no),
               u2nc(1, u2_yes));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, flip)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun hel;

    if ( u2_none == (hel = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, flip)(hel);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, flip)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, flip), Tier6_a, u2_none, u2_none },
    { }
  };
