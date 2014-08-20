/* j/6/tack.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun
  _tack_in(u2_noun, u2_noun, u2_noun, u2_noun, u2_atom);


/* internals
*/
  static u2_noun                                                  //  transfer
  _tack_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun peh,                                           //  retain
           u2_noun mur,                                           //  retain
           u2_atom axe)                                           //  retain
  {
    c3_assert(0);

    if ( u2_no == u2du(peh) ) {
      return u2nc(u2k(axe), u2k(mur));
    }
    else {
      u2_noun i_peh = u2h(peh);
      u2_noun t_peh = u2t(peh);

      if ( (u2_yes == u2ud(i_peh)) || (u2_no == u2h(i_peh)) )
      {
        u2_noun pi_peh = (u2_yes == u2ud(i_peh)) ? 0 : u2h(u2t(i_peh));
        u2_noun qi_peh = (u2_yes == u2ud(i_peh)) ? i_peh : u2t(u2t(i_peh));
        u2_noun pok   = j2_mcy(Pt6, ut, fink)
                              (van, sut, pi_peh, c3__rite, qi_peh);
        u2_noun wuf   = j2_mby(Pt6, flay)(pok);
        u2_noun p_wuf = u2h(wuf);
        u2_noun q_wuf = u2t(wuf);
        u2_noun nax   = j2_mbc(Pt3, peg)(axe, p_wuf);
        u2_noun gav   = _tack_in(van, q_wuf, t_peh, mur, nax);
        u2_noun p_gav = u2h(gav);
        u2_noun q_gav = u2t(gav);
        u2_noun qog   = u2nc(u2_nul, u2k(qi_peh));
        u2_noun ret   = u2nc
          (u2k(p_gav),
                  j2_mcy(Pt6, ut, heal)(van, sut, qog, p_wuf, q_gav));

        u2z(qog);
        u2z(gav);
        u2z(nax);
        u2z(wuf);
        u2z(pok);

        return ret;
      }
      else {
        u2_noun bax   = u2t(i_peh);
        u2_noun vas   = j2_mcy(Pt6, ut, peek)(van, sut, c3__rite, bax);
        u2_noun nax   = j2_mbc(Pt3, peg)(axe, bax);
        u2_noun gav   = _tack_in(van, vas, t_peh, mur, nax);
        u2_noun p_gav = u2h(gav);
        u2_noun q_gav = u2t(gav);
        u2_noun ret   = u2nc
          (u2k(p_gav),
                  j2_mcy(Pt6, ut, heal)(van, sut, u2_nul, bax, q_gav));

        u2z(gav);
        u2z(nax);
        u2z(vas);

        return ret;
      }
    }
  }

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, tack)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    return _tack_in(van, sut, peh, mur, 1);
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, tack)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, tack)(
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, peh, mur;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &peh,
                                u2_cv_sam_3, &mur,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, tack)(van, sut, peh, mur);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, tack)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    u2_weak hoc = u2_cj_look(van, "tack");

    if ( u2_none == hoc ) {
      c3_assert(!"register tack");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam_2, u2k(peh),
                                           u2_cv_sam_3, u2k(mur),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, tack)[0].xip) ) {
        u2_noun xip = u2_cj_find(cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, tack)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, tack)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, tack)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, tack)(van, sut, peh, mur);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, tack)(van, sut, peh, mur);
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
  j2_mcj(Pt6, ut, tack)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, tack),
      u2_jet_dead,
      // Tier6_b,
      u2_none, u2_none },
    { }
  };
