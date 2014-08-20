/* j/6/flan.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, flan)(
                    u2_noun bos,                                  //  retain
                    u2_noun nif)                                  //  retain
  {
    if ( u2_yes == u2_cr_sing(1, u2h(bos)) ) {
      if ( (u2_nul == u2t(bos)) ) {
        return u2k(nif);
      }
      else return u2k(bos);
    }
    else {
      if ( u2_yes == u2_cr_sing(1, u2h(nif)) ) {
        if ( (u2_nul == u2t(nif)) ) {
          return u2k(bos);
        }
        else return u2k(nif);
      }
      else {
        return u2nq
          (6,
                  u2k(bos),
                  u2k(nif),
                  u2nc(1, u2_no));
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, flan)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun bos, nif;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &bos, u2_cv_sam_3, &nif, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, flan)(bos, nif);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, flan)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, flan), Tier6_a, u2_none, u2_none },
    { }
  };
