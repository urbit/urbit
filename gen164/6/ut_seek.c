/* j/6/seek.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun                                                  //  produce
  _seek_flat(
             u2_noun wob)                                         //  retain
  {
    if ( u2_nul == wob ) {
      return u2_nul;
    } else {
      u2_noun i_wob = u2h(wob);
      u2_noun t_wob = u2t(wob);

      return u2nc
        (u2nc(u2k(u2h(i_wob)),
                             u2nt(c3__ash, u2_nul, 1)),
                _seek_flat(t_wob));
    }
  }

#if 0
  static u2_noun                                                  //  transfer
  _seek_silk_yew(
                 u2_noun van,                                     //  retain
                 u2_noun syx,                                     //  retain
                 u2_noun qq_tor)                                  //  retain
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    else {
      u2_noun iqq_tor  = u2h(qq_tor);
      u2_noun qiqq_tor = u2t(iqq_tor);
      u2_noun yon      = _seek_silk_yew(van, syx, u2t(qq_tor));

      if ( c3__yew != u2h(qiqq_tor) ) {
        return yon;
      } else {
        u2_noun nuy = j2_mby(Pt6, look)(syx, u2t(qiqq_tor));

        if ( u2_nul == nuy ) {
          return u2_cm_error("silk");
        }
        else {
          yon = u2nc(u2k(u2t(nuy)), yon);
          u2z(nuy);
          return yon;
        }
      }
    }
  }
  static u2_noun                                                  //  retain
  _seek_silk_yaw(u2_noun 
                 u2_noun hey)                                     //  retain
  {
    u2_atom axe = 0;

    while ( u2_nul != hey ) {
      if ( axe == 0 ) {
        axe = u2h(u2h(hey));
      } else if ( axe != u2h(u2h(hey)) ) {
        return u2_cm_error("silk");
      }
      hey = u2t(hey);
    }
  }

  static u2_noun                                                  //  transfer
  _seek_silk_fum(u2_noun 
                 u2_noun hey,                                     //  retain
                 u2_noun qq_tor)
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    c3_assert(u2_nul != hey);
    return u2nc
      (u2nc(u2k(u2h(u2h(qq_tor))),
                           u2k(u2t(u2h(hey)))),
              _seek_silk_fum(u2t(hey), u2t(qq_tor)));
  }

  static u2_noun                                                  //  transfer
  _seek_silk(
             u2_noun van,                                         //  retain
             u2_noun syx,                                         //  retain
             u2_noun tor)                                         //  retain
  {
    u2_noun p_tor, q_tor, pq_tor, qq_tor;
    u2_noun hey, ret;

    u2_cr_cell(tor, &p_tor, &q_tor);
    if ( u2_yes == u2h(q_tor) ) {
      return u2_nul;
    }
    u2_cr_cell(u2t(q_tor), &pq_tor, &qq_tor);

    hey = _seek_silk_yew(van, syx, qq_tor);
    if ( u2_nul == hey ) {
      return u2_nul;
    }
    if ( u2_ckb_lent(u2k(hey)) !=
         u2_ckb_lent(u2k(qq_tor)) )
    {
      return u2_cm_error("silk");
    }

    ret = u2nq
      (u2_nul,
              u2_no,
              j2_mbc(Pt3, peg)(pq_tor, _seek_silk_yaw(hey)),
              _seek_silk_fum(hey, qq_tor));

    u2z(hey);
    return ret;
  }
#endif

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, seek)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    if ( u2_nul == hyp ) {
      return u2nt
        (1, u2_yes, u2k(sut));
    }
    else if ( u2_no == u2du(hyp) ) {
      return u2_cm_bail(c3__fail);
    }
    else {
      u2_noun i_hyp = u2h(hyp);
      u2_noun t_hyp = u2t(hyp);
      u2_noun zar;
      u2_noun p_zar, q_zar;
      u2_noun yip, syp, ret;

      if ( u2_yes == u2du(i_hyp) ) {
        yip = u2k(i_hyp);
      } else {
        yip = u2nt(u2_no, 0, u2k(i_hyp));
      }

      zar = j2_mcx(Pt6, ut, seek)(van, sut, way, t_hyp);
      u2_cr_cell(zar, &p_zar, &q_zar);

#if 0
      if ( u2_yes == u2h(yip) ) {
        sic = u2_nul;
      } else {
        // sic = _seek_silk(van, u2h(u2t(yip)), zar);
        sic = u2_nul;
      }
      if ( u2_nul != sic ) {
        u2z(yip);
        u2z(zar);

        return u2t(sic);
      }
#endif

      if ( u2_yes == u2h(q_zar) ) {
        syp = u2k(u2t(q_zar));
      } else {
        u2_noun pq_zar, qq_zar;
        u2_noun wip;

        u2_cr_cell(u2t(q_zar), &pq_zar, &qq_zar);
        wip = _seek_flat(qq_zar);
        syp = j2_mcy(Pt6, ut, fire)(van, sut, wip);

        u2z(wip);
      }

      if ( u2_no == u2h(yip) ) {
        u2_noun p_yip, q_yip, hud;

        if ( u2_no == u2_cr_cell(u2t(yip), &p_yip, &q_yip) ) {
          return u2_cm_bail(c3__fail);
        }
        hud = j2_mcy(Pt6, ut, fink)(van, syp, p_yip, way, q_yip);
        {
          u2_noun p_hud, q_hud;

          u2_cr_cell(hud, &p_hud, &q_hud);

          ret = u2nc(j2_mbc(Pt3, peg)(p_zar, p_hud),
                             u2k(q_hud));
          u2z(hud);
        }
      }
      else {
        u2_noun p_yip = u2t(yip);

        if ( u2_no == u2ud(p_yip) ) {
          return u2_cm_bail(c3__fail);
        }
        else {
          ret = u2nt
            (j2_mbc(Pt3, peg)(p_zar, p_yip),
                    u2_yes,
                    j2_mcy(Pt6, ut, peek)(van, syp, way, p_yip));
        }
      }
      u2z(yip);
      u2z(syp);
      u2z(zar);
      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, seek)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, seek)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, hyp, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &way,
                                u2_cv_sam_3, &hyp,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, seek)(van, sut, way, hyp);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, seek)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_weak hoc = u2_cj_look(van, "seek");

    if ( u2_none == hoc ) {
      c3_assert(!"register seek");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam_2, u2k(way),
                                      u2_cv_sam_3, u2k(hyp),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, seek)[0].xip) ) {
        u2_noun xip = u2_cj_find(cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, seek)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, seek)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, seek)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, seek)(van, sut, way, hyp);
      }
      else {
        c3_m    fun_m = c3__seek;
        u2_noun pro   = u2_ch_find_3(fun_m, sut, way, hyp);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, seek)(van, sut, way, hyp);

          return u2_ch_save_3(fun_m, sut, way, hyp, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, seek)(van, sut, way, hyp);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, seep)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_noun zar = j2_mcy(Pt6, ut, seek)(van, sut, way, hyp);
    u2_noun p_zar = u2h(zar);
    u2_noun q_zar = u2t(zar);

    if ( u2_yes != u2h(q_zar) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun ret = u2nc(u2k(p_zar),
                                 u2k(u2t(q_zar)));

      u2z(zar);
      return ret;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, seek)(
                        u2_noun cor)
  {
    u2_noun sut, way, hyp, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &way,
                                u2_cv_sam_3, &hyp,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nt
        (u2k(sut), u2k(way), u2k(hyp));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, seek)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, seek),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, seek), c3__seek
    },
    { }
  };

