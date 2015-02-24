/* j/6/swab.c
**
*/
#include "all.h"


/* internals
*/

/* functions
*/
  u3_noun
  _cqfu_snub(u3_noun van,
                        u3_noun sut,
                        u3_noun har)
  {
    if ( c3n == u3du(har) ) {
      return u3_nul;
    } else {
      u3_noun i_har = u3h(har);
      u3_noun t_har = u3t(har);
      u3_noun pi_har = u3h(i_har);
      u3_noun qi_har = u3t(i_har);
      u3_noun peh = u3qb_flop(pi_har);
      u3_noun ret = u3nc(u3nc(peh, u3k(qi_har)),
                                 _cqfu_snub
                                    (van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_snub(
                       u3_noun cor)
  {
    u3_noun van, sut, har;

    if ( (c3n == u3r_mean(cor, u3x_sam, &har,
                                u3x_con, &van,
                                0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_snub(van, sut, har);
    }
  }

  u3_noun
  u3qfu_snub(u3_noun van,
                        u3_noun sut,
                        u3_noun har)
  {
    return _cqfu_snub(van, sut, har);
  }

