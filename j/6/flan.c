/* j/6/flan.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_flan(
                    u3_noun bos,
                    u3_noun nif)
  {
    if ( c3y == u3_cr_sing(1, u3h(bos)) ) {
      if ( (u3_nul == u3t(bos)) ) {
        return u3k(nif);
      }
      else return u3k(bos);
    }
    else {
      if ( c3y == u3_cr_sing(1, u3h(nif)) ) {
        if ( (u3_nul == u3t(nif)) ) {
          return u3k(bos);
        }
        else return u3k(nif);
      }
      else {
        return u3nq
          (6,
                  u3k(bos),
                  u3k(nif),
                  u3nc(1, c3n));
      }
    }
  }
  u3_noun
  u3_cwf_flan(
                   u3_noun cor)
  {
    u3_noun bos, nif;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &bos, u3_cv_sam_3, &nif, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_flan(bos, nif);
    }
  }
