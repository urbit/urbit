/* j/6/tock.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/
  static u2_noun                                                  //  transfer
  _tock_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun peh,                                           //  retain
           u2_noun mur,                                           //  retain
           u2_noun men)                                           //  retain
  {
    if ( u2_no == u2du(men) ) {
      return u2nc(u2_nul, u2_nul);
    }
    else {
      u2_noun i_men  = u2h(men);
      u2_noun pi_men = u2h(i_men);
      u2_noun qi_men = u2t(i_men);
      u2_noun t_men  = u2t(men);
      u2_noun geq    = j2_mcy(Pt6, ut, tack)(van, pi_men, peh, mur);
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
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, tock)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
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
  u2_ho_jet
  j2_mcj(Pt6, ut, tock)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, tock)(
                       u2_noun cor)                               //  retain
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
      return j2_mcx(Pt6, ut, tock)(van, sut, peh, mur, men);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, tock)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "tock");

    if ( u2_none == hoc ) {
      c3_assert(!"register tock");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam_2, u2k(peh),
                                           u2_cv_sam_6, u2k(mur),
                                           u2_cv_sam_7, u2k(men),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, tock)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, tock)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, tock)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, tock)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, tock)(van, sut, peh, mur, men);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, tock)(van, sut, peh, mur, men);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, tock)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, tock), Tier6_c, u2_none, u2_none },
    { }
  };
