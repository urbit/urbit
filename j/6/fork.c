/* j/6/fork.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_fork(
                    u3_noun hoz,
                    u3_noun bur)
  {
    if ( c3y == u3_cr_sing(hoz, bur) ) {
      return u3k(hoz);
    }
    else if ( c3__void == bur ) {
      return u3k(hoz);
    }
    else if ( c3__void == hoz ) {
      return u3k(bur);
    }
    else return u3nt
      (c3__fork, u3k(hoz), u3k(bur));
  }
  u3_noun
  u3_cwf_fork(
                   u3_noun cor)
  {
    u3_noun hoz, bur;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &hoz, u3_cv_sam_3, &bur, 0) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_fork(hoz, bur);
    }
  }
