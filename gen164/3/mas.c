/* j/3/mas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqc_mas(
                   u2_atom a)                                     //  retain
  {
    c3_w b_w;
    u2_atom c, d, e, f;

    b_w = u2_cr_met(0, a);
    if ( b_w < 2 ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      c = u2_cqc_bex((b_w - 1));
      d = u2_cqc_bex((b_w - 2));
      e = u2_cqa_sub(a, c);
      f = u2_cqc_con(e, d);

      u2z(c);
      u2z(d);
      u2z(e);

      return f;
    }
  }
  u2_weak                                                         //  transfer
  u2_cwc_mas(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_mas(a);
    }
  }

