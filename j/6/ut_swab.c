/* j/6/swab.c
**
** This file is in the public domain.
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
      u3_noun peh = u3_cqb_flop(pi_har);
      u3_noun ret = u3nc(u3nc(peh, u3k(qi_har)),
                                 _cqfu_snub
                                    (van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_snub(
                       u3_noun cor)
  {
    u3_noun van, sut, har;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam, &har,
                                u3_cv_con, &van,
                                0)) ||
         (c3nne == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_snub(van, sut, har);
    }
  }

  u3_noun
  u3_cqfu_snub(u3_noun van,
                        u3_noun sut,
                        u3_noun har)
  {
    return _cqfu_snub(van, sut, har);
  }

