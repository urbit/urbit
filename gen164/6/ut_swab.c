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
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, snub)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    if ( u2_no == u2du(har) ) {
      return u2_nul;
    } else {
      u2_noun i_har = u2h(har);
      u2_noun t_har = u2t(har);
      u2_noun pi_har = u2h(i_har);
      u2_noun qi_har = u2t(i_har);
      u2_noun peh = j2_mbc(Pt2, flop)(pi_har);
      u2_noun ret = u2nc(u2nc(peh, u2k(qi_har)),
                                 j2_mcx(Pt6, ut, snub)
                                    (van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, snub)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, snub)(
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, har;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &har,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, snub)(van, sut, har);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, snub)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "snub");

    if ( u2_none == hoc ) {
      c3_assert(!"register snub");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(har),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, snub)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, snub)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, snub)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, snub)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, snub)(van, sut, har);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, snub)(van, sut, har);
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
  j2_mcj(Pt6, ut, snub)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, snub), Tier6_c, u2_none, u2_none },
    { }
  };
