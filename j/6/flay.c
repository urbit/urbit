/* j/6/flay.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _flay_roll(
             u3_noun quz)
  {
    if ( u3_no == u3du(quz) ) {
      return c3__void;
    } else {
      u3_noun voo = _flay_roll(u3t(quz));
      u3_noun oon = u3_cqf_fork(u3h(u3h(quz)), voo);

      u3z(voo);
      return oon;
    }
  }

  u3_noun
  u3_cqf_flay(
                    u3_noun pok)
  {
    u3_noun p_pok = u3h(pok);
    u3_noun q_pok = u3t(pok);
    u3_noun typ;

    switch ( u3h(q_pok) ) {
      default: return u3_cm_bail(c3__fail);

      case u3_yes: typ = u3k(u3t(q_pok));
                   break;
      case u3_no: typ = _flay_roll(u3t(u3t(q_pok)));
                  break;
    }
    return u3nc(u3k(p_pok), typ);
  }

  u3_noun
  u3_cwf_flay(
                   u3_noun cor)
  {
    u3_noun pok;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &pok, 0)) ||
         (u3_no == u3du(pok)) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_flay(pok);
    }
  }
