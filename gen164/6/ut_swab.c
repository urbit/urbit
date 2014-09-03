/* j/6/swab.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/

/* functions
*/
  u2_noun
  _cqfu_snub(u2_noun van,
                        u2_noun sut,
                        u2_noun har)
  {
    if ( u2_no == u2du(har) ) {
      return u2_nul;
    } else {
      u2_noun i_har = u2h(har);
      u2_noun t_har = u2t(har);
      u2_noun pi_har = u2h(i_har);
      u2_noun qi_har = u2t(i_har);
      u2_noun peh = u2_cqb_flop(pi_har);
      u2_noun ret = u2nc(u2nc(peh, u2k(qi_har)),
                                 _cqfu_snub
                                    (van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_snub(
                       u2_noun cor)
  {
    u2_noun van, sut, har;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &har,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_snub(van, sut, har);
    }
  }

  u2_noun
  u2_cqfu_snub(u2_noun van,
                        u2_noun sut,
                        u2_noun har)
  {
    return _cqfu_snub(van, sut, har);
  }

