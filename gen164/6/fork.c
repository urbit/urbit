/* j/6/fork.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqf_fork(
                    u2_noun hoz,
                    u2_noun bur)
  {
    if ( u2_yes == u2_cr_sing(hoz, bur) ) {
      return u2k(hoz);
    }
    else if ( c3__void == bur ) {
      return u2k(hoz);
    }
    else if ( c3__void == hoz ) {
      return u2k(bur);
    }
    else return u2nt
      (c3__fork, u2k(hoz), u2k(bur));
  }
  u2_noun
  u2_cwf_fork(
                   u2_noun cor)
  {
    u2_noun hoz, bur;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &hoz, u2_cv_sam_3, &bur, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_fork(hoz, bur);
    }
  }
