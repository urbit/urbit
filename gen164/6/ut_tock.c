/* j/6/tock.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/
  static u2_noun
  _tock_in(
           u2_noun van,
           u2_noun sut,
           u2_noun peh,
           u2_noun mur,
           u2_noun men)
  {
    if ( u2_no == u2du(men) ) {
      return u2nc(u2_nul, u2_nul);
    }
    else {
      u2_noun i_men  = u2h(men);
      u2_noun pi_men = u2h(i_men);
      u2_noun qi_men = u2t(i_men);
      u2_noun t_men  = u2t(men);
      u2_noun geq    = u2_cqfu_tack(van, pi_men, peh, mur);
      u2_noun p_geq  = u2h(geq);
      u2_noun q_geq  = u2t(geq);
      u2_noun mox    = _tock_in(van, sut, peh, mur, t_men);
      u2_noun p_mox  = u2h(mox);
      u2_noun q_mox  = u2t(mox);
      u2_noun ret;

      ret = u2nc(
                  ( (u2_nul == p_mox)
                      ? u2nc(u2_nul, u2k(p_geq))
                      : (u2_no == u2_cr_sing(p_geq, u2t(p_mox)))
                        ? u2_cm_bail(c3__exit)
                        : u2k(p_mox) ),
                  u2nc(u2nc(u2k(q_geq),
                                            u2k(qi_men)),
                               u2k(q_mox)));

      u2z(mox);
      u2z(geq);
      return ret;
    }
  }

/* functions
*/
  u2_noun
  _cqfu_tock(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun peh,
                        u2_noun mur,
                        u2_noun men)
  {
    u2_noun wib = _tock_in(van, sut, peh, mur, men);
    u2_noun p_wib = u2h(wib);
    u2_noun q_wib = u2t(wib);

    if ( u2_nul == p_wib ) {
      return u2_cm_bail(c3__exit);
    } else {
      u2_noun ret = u2nc(u2k(u2t(p_wib)),
                                 u2k(q_wib));

      u2z(wib);
      return ret;
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_tock(
                       u2_noun cor)
  {
    u2_noun van, sut, peh, mur, men;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &peh,
                                u2_cv_sam_6, &mur,
                                u2_cv_sam_7, &men,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_tock(van, sut, peh, mur, men);
    }
  }

  u2_noun
  u2_cqfu_tock(u2_noun van,
                        u2_noun sut,
                        u2_noun peh,
                        u2_noun mur,
                        u2_noun men)
  {
    return _cqfu_tock(van, sut, peh, mur, men);
  }
