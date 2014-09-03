/* j/3/peg.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqc_peg(
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    u2_atom c, d, e, f, g, h;

    c = u2_cr_met(0, b);
    d = u2_cqa_dec(c);
    e = u2_cqc_lsh(0, d, 1);
    f = u2_cqa_sub(b, e);
    g = u2_cqc_lsh(0, d, a);
    h = u2_cqa_add(f, g);

    u2z(c);
    u2z(d);
    u2z(e);
    u2z(f);
    u2z(g);

    return h;
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, peg)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) ||
         (0 == a) ||
         (0 == b) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_peg(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, peg)[] = {
    { ".2", c3__lite, j2_mb(Pt3, peg), Tier3, u2_none, u2_none },
    { }
  };
