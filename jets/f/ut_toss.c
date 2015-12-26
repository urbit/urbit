/* j/6/toss.c
**
*/
#include "all.h"


/* internals
*/
  static u3_noun
  _toss_in(u3_noun van,
           u3_noun sut,
           u3_noun peh,
           u3_noun mur,
           u3_noun men)
  {
    if ( c3n == u3du(men) ) {
      return u3nc(u3_nul, u3_nul);
    }
    else {
      u3_noun i_men  = u3h(men);
      u3_noun pi_men = u3h(i_men);
      u3_noun qi_men = u3t(i_men);
      u3_noun t_men  = u3t(men);
      u3_noun geq    = u3qfu_tack(van, pi_men, peh, mur);
      u3_noun p_geq  = u3h(geq);
      u3_noun q_geq  = u3t(geq);
      u3_noun mox    = _toss_in(van, sut, peh, mur, t_men);
      u3_noun p_mox  = u3h(mox);
      u3_noun q_mox  = u3t(mox);
      u3_noun ret;

      ret = u3nc(
                  ( (u3_nul == p_mox)
                      ? u3nc(u3_nul, u3k(p_geq))
                      : (c3n == u3r_sing(p_geq, u3t(p_mox)))
                        ? u3m_bail(c3__exit)
                        : u3k(p_mox) ),
                  u3nc(u3nc(u3k(q_geq), u3k(qi_men)), u3k(q_mox)));

      u3z(mox);
      u3z(geq);
      return ret;
    }
  }

/* functions
*/
  u3_noun
  _cqfu_toss(u3_noun van,
             u3_noun sut,
             u3_noun peh,
             u3_noun mur,
             u3_noun men)
  {
    u3_noun wib = _toss_in(van, sut, peh, mur, men);
    u3_noun p_wib = u3h(wib);
    u3_noun q_wib = u3t(wib);

    if ( u3_nul == p_wib ) {
      return u3m_bail(c3__exit);
    } else {
      u3_noun ret = u3nc(u3k(u3t(p_wib)), u3k(q_wib));

      u3z(wib);
      return ret;
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_toss(u3_noun cor)
  {
    u3_noun van, sut, peh, mur, men;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &peh,
                               u3x_sam_6, &mur,
                               u3x_sam_7, &men,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_toss(van, sut, peh, mur, men);
    }
  }

  u3_noun
  u3qfu_toss(u3_noun van,
             u3_noun sut,
             u3_noun peh,
             u3_noun mur,
             u3_noun men)
  {
    return _cqfu_toss(van, sut, peh, mur, men);
  }
