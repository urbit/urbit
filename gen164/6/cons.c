/* j/6/cons.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, cons)(
                    u2_noun vur,                                  //  retain
                    u2_noun sed)                                  //  retain
  {
    u2_noun p_vur, p_sed;

    if ( u2_yes == u2_cr_p(vur, 1, &p_vur) &&
         u2_yes == u2_cr_p(sed, 1, &p_sed) ) {
      return u2nt(1,
                          u2k(p_vur),
                          u2k(p_sed));
    }
    else if ( u2_yes == u2_cr_p(vur, 0, &p_vur) &&
              u2_yes == u2_cr_p(sed, 0, &p_sed) &&
              !(u2_yes == u2_cr_sing(1, p_vur)) &&
              !(u2_yes == u2_cr_sing(p_vur, p_sed)) &&
              (0 == u2_cr_nord(p_vur, p_sed)) )
    {
      u2_atom fub = j2_mbc(Pt1, div)(p_vur, 2);
      u2_atom nof = j2_mbc(Pt1, div)(p_sed, 2);

      if ( u2_yes == u2_cr_sing(fub, nof) ) {
        u2z(nof);

        return u2nc(0, fub);
      }
      else {
        u2z(fub);
        u2z(nof);
      }
    }
    return u2nc(u2k(vur), u2k(sed));
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, cons)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun vur, sed;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vur, u2_cv_sam_3, &sed, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, cons)(vur, sed);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, cons)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, cons), Tier6_a, u2_none, u2_none },
    { }
  };
