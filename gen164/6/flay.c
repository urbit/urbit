/* j/6/flay.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun                                                  //  produce
  _flay_roll(
             u2_noun quz)                                         //  retain
  {
    if ( u2_no == u2du(quz) ) {
      return c3__void;
    } else {
      u2_noun voo = _flay_roll(u2t(quz));
      u2_noun oon = j2_mby(Pt6, fork)(u2h(u2h(quz)), voo);

      u2z(voo);
      return oon;
    }
  }

  u2_noun                                                         //  produce
  j2_mby(Pt6, flay)(
                    u2_noun pok)                                  //  retain
  {
    u2_noun p_pok = u2h(pok);
    u2_noun q_pok = u2t(pok);
    u2_noun typ;

    switch ( u2h(q_pok) ) {
      default: return u2_cm_bail(c3__fail);

      case u2_yes: typ = u2k(u2t(q_pok));
                   break;
      case u2_no: typ = _flay_roll(u2t(u2t(q_pok)));
                  break;
    }
    return u2nc(u2k(p_pok), typ);
  }

  u2_noun                                                         //  transfer
  j2_mb(Pt6, flay)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun pok;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &pok, 0)) ||
         (u2_no == u2du(pok)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt6, flay)(pok);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, flay)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, flay), Tier6_a, u2_none, u2_none },
    { }
  };
