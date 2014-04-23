/* j/6/flay.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun                                                  //  produce
  _flay_roll(u2_wire wir_r,
             u2_noun quz)                                         //  retain
  {
    if ( u2_no == u2_dust(quz) ) {
      return c3__void;
    } else {
      u2_noun voo = _flay_roll(wir_r, u2_t(quz));
      u2_noun oon = j2_mby(Pt6, fork)(wir_r, u2_h(u2_h(quz)), voo);

      u2_rz(wir_r, voo);
      return oon;
    }
  }

  u2_noun                                                         //  produce
  j2_mby(Pt6, flay)(u2_wire wir_r,
                    u2_noun pok)                                  //  retain
  {
    u2_noun p_pok = u2_h(pok);
    u2_noun q_pok = u2_t(pok);
    u2_noun typ;

    switch ( u2_h(q_pok) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case u2_yes: typ = u2_rx(wir_r, u2_t(q_pok));
                   break;
      case u2_no: typ = _flay_roll(wir_r, u2_t(u2_t(q_pok)));
                  break;
    }
    return u2_bc(wir_r, u2_rx(wir_r, p_pok), typ);
  }

  u2_noun                                                         //  transfer
  j2_mb(Pt6, flay)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun pok;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &pok, 0)) ||
         (u2_no == u2_dust(pok)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, flay)(wir_r, pok);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, flay)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, flay), Tier6_a, u2_none, u2_none },
    { }
  };
