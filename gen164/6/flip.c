/* j/6/flip.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqf_flip(
                    u2_noun hel)
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
  u2_noun
  u2_cwf_flip(
                   u2_noun cor)
  {
    u2_noun hel;

    if ( u2_none == (hel = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_flip(hel);
    }
  }
