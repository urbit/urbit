/* j/2/roll.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_roll(u3_noun a,
                    u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(u3_cr_at(u3_cv_sam_3, b));
    }
    else if ( c3n == u3du(a) ) {
      return u3_cm_bail(c3__exit);
    }
    else {
      u3_noun gim = u3k(u3h(a));
      u3_noun zor = u3k(u3_cr_at(u3_cv_sam_3, b));
      u3_noun daz = u3_cn_slam_on(u3k(b), u3nc(gim, zor));
      u3_noun vel = u3_ci_molt(u3k(b), u3_cv_sam_3, daz, 0);

      if ( u3_none == vel ) {
        return u3_cm_bail(c3__exit);
      } else {
        u3_noun hox = u3_cqb_roll(u3t(a), vel);

        u3z(vel);
        return hox;
      }
    }
  }
  u3_noun
  u3_cwb_roll(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_roll(a, b);
    }
  }

