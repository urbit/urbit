/* j/3/dor.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_dor(
                   u3_atom a,
                   u3_atom b)
  {
    if ( u3_yes == u3_cr_sing(a, b) ) {
      return u3_yes;
    }
    else {
      if ( u3_yes == u3ud(a) ) {
        if ( u3_yes == u3ud(b) ) {
          return u3_cqa_lth(a, b);
        }
        else {
          return u3_yes;
        }
      }
      else {
        if ( u3_yes == u3ud(b) ) {
          return u3_no;
        }
        else {
          if ( u3_yes == u3_cr_sing(u3h(a), u3h(b)) ) {
            return u3_cqc_dor(u3t(a), u3t(b));
          }
          else return u3_cqc_dor(u3h(a), u3h(b));
        }
      }
    }
  }
  u3_noun
  u3_cwc_dor(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_dor(a, b);
    }
  }

