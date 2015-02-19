/* j/6/flay.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _flay_roll(
             u3_noun quz)
  {
    if ( c3n == u3du(quz) ) {
      return c3__void;
    } else {
      u3_noun voo = _flay_roll(u3t(quz));
      u3_noun oon = u3qf_fork(u3h(u3h(quz)), voo);

      u3z(voo);
      return oon;
    }
  }

  u3_noun
  u3qf_flay(
                    u3_noun pok)
  {
    u3_noun p_pok = u3h(pok);
    u3_noun q_pok = u3t(pok);
    u3_noun typ;

    switch ( u3h(q_pok) ) {
      default: return u3m_bail(c3__fail);

      case c3y: typ = u3k(u3t(q_pok));
                   break;
      case c3n: typ = _flay_roll(u3t(u3t(q_pok)));
                  break;
    }
    return u3nc(u3k(p_pok), typ);
  }

  u3_noun
  u3wf_flay(
                   u3_noun cor)
  {
    u3_noun pok;

    if ( (c3n == u3r_mean(cor, u3x_sam, &pok, 0)) ||
         (c3n == u3du(pok)) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_flay(pok);
    }
  }
