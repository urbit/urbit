/* j/3/dor.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqc_dor(
                   u2_atom a,
                   u2_atom b)
  {
    if ( u2_yes == u2_cr_sing(a, b) ) {
      return u2_yes;
    }
    else {
      if ( u2_yes == u2ud(a) ) {
        if ( u2_yes == u2ud(b) ) {
          return u2_cqa_lth(a, b);
        }
        else {
          return u2_yes;
        }
      }
      else {
        if ( u2_yes == u2ud(b) ) {
          return u2_no;
        }
        else {
          if ( u2_yes == u2_cr_sing(u2h(a), u2h(b)) ) {
            return u2_cqc_dor(u2t(a), u2t(b));
          }
          else return u2_cqc_dor(u2h(a), u2h(b));
        }
      }
    }
  }
  u2_noun
  u2_cwc_dor(
                  u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_dor(a, b);
    }
  }

