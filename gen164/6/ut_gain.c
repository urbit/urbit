/* j/6/gain.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun
  j2_mcx(Pt6, ut, gain)(u2_noun, u2_noun, u2_noun);

  static u2_noun                                                  //  produce
  _gain_wtpm(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun p_gen)                                       //  retain
  {
    if ( u2_no == u2du(p_gen) ) {
      return u2k(sut);
    }
    else {
      u2_noun ip_gen = u2h(p_gen);
      u2_noun tp_gen = u2t(p_gen);
      u2_noun lev = j2_mcx(Pt6, ut, gain)(van, sut, ip_gen);
      u2_noun pro = _gain_wtpm(van, lev, tp_gen);

      u2z(lev);
      return pro;
    }
  }

  static u2_noun                                                  //  produce
  _gain_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun gen)                                           //  retain
  {
    u2_noun p_gen, q_gen;

    if ( u2_no == u2du(gen) ) {
      return u2k(sut);
    } else switch ( u2h(gen) ) {
      default: return u2k(sut);

      case c3__wtts: {
        if ( u2_no == u2_cr_mean(gen, 6, &p_gen, 7, &q_gen, 0) ) {
          return u2_cm_bail(c3__fail);
        } else {
          u2_noun rac = j2_mcy(Pt6, ap, rake)(q_gen);
          u2_noun hap = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
          u2_noun guz = j2_mcy(Pt6, ut, seek)(van, sut, c3__read, rac);
          u2_noun axe = u2h(guz);
          u2_noun ret = j2_mcy(Pt6, ut, cull)
            (van, sut, u2_yes, axe, hap);

          u2z(guz);
          u2z(hap);
          u2z(rac);

          return ret;
        }
      }
      case c3__wtpm: {
        return _gain_wtpm(van, sut, u2t(gen));
      }
      case c3__zpcb: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      {
        return j2_mcx(Pt6, ut, gain)(van, sut, q_gen);
      }
      case c3__zphs: p_gen = u2t(gen);
      {
        return j2_mcx(Pt6, ut, gain)(van, sut, p_gen);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, gain)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    return _gain_in(van, sut, gen);
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, gain)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, gain)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &gen, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, gain)(van, sut, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, gain)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "gain");

    if ( u2_none == hoc ) {
      c3_assert(!"register gain");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(gen), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, gain)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, gain)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, gain)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, gain)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, gain)(van, sut, gen);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, gain)(van, sut, gen);
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
  j2_mcj(Pt6, ut, gain)[] = {
    //  Dead for the moment because wtts behavior is wrong & bore is not
    //  available.
    //
    // { ".2", c3__hevy, j2_mc(Pt6, ut, gain), Tier6_c, u2_none, u2_none },
    { ".2", c3__hevy, j2_mc(Pt6, ut, gain), u2_jet_dead, u2_none, u2_none },
    { }
  };
