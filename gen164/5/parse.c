/* j/5/parse.c
**
** This file is in the public domain.
*/
#include "all.h"


  static u2_noun
  _slip(
        u2_noun weq,
        u2_noun naz)
  {
    u2_noun p_naz, q_naz;

    u2_cx_cell(naz, &p_naz, &q_naz);
    if ( 10 == weq ) {
      return u2nc(u2_cx_good(
                                     u2_ci_vint(u2k(p_naz))),
                          1);
    } else {
      return u2nc(u2k(p_naz),
                          u2_cx_good(
                                     u2_ci_vint(u2k(q_naz))));
    }
  }

  static u2_noun
  _fail(
        u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);
    return u2nc(u2k(p_tub), u2_nul);
  }

  static u2_noun
  _last(
        u2_noun zyc,
        u2_noun naz)
  {
    u2_noun p_zyc, q_zyc, p_naz, q_naz;

    u2_cx_cell(zyc, &p_zyc, &q_zyc);
    u2_cx_cell(naz, &p_naz, &q_naz);

    if ( u2_ne(u2_co_is_cat(p_zyc)) || u2_ne(u2_co_is_cat(q_zyc)) ||
         u2_ne(u2_co_is_cat(p_naz)) || u2_ne(u2_co_is_cat(q_naz)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      if ( p_zyc == p_naz ) {
        return (q_zyc > q_naz) ? u2k(zyc) : u2k(naz);
      }
      else {
        return (p_zyc > p_naz) ? u2k(zyc) : u2k(naz);
      }
    }
  }

  static u2_noun
  _next(
        u2_noun tub)
  {
    u2_noun p_tub, q_tub;
    u2_noun zac;

    u2_cx_cell(tub, &p_tub, &q_tub);
    if ( u2_no == u2du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u2_noun iq_tub = u2h(q_tub);
      u2_noun tq_tub = u2t(q_tub);

      zac = _slip(iq_tub, p_tub);

      return u2nc
        (zac,
                u2nq(u2_nul,
                             u2k(iq_tub),
                             u2k(zac),
                             u2k(tq_tub)));
    }
  }

/* bend
*/
  u2_noun
  _cqe_bend_fun(u2_noun raq,
                         u2_noun vex,
                         u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    } else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u2_no == u2du(q_yit) ) {
        ret = u2nc(yur, u2k(q_vex));
      }
      else {
        u2_noun uq_yit = u2t(q_yit);
        u2_noun puq_yit, quq_yit;
        u2_noun vux;

        u2_cx_cell(uq_yit, &puq_yit, &quq_yit);

        vux = u2_cx_good
                    (u2_cn_slam_on
                       (u2k(raq),
                               u2nc(u2k(puq_vex),
                                            u2k(puq_yit))));
        if ( u2_nul == vux ) {
          ret = u2nc(yur, u2k(q_vex));
        }
        else {
          ret = u2nq(yur,
                             u2_nul,
                             u2k(u2t(vux)),
                             u2k(quq_yit));
          u2z(vux);
        }
      }
      u2z(yit);
      return ret;
    }
  }

  u2_noun
 u2_cwe_bend_fun(u2_noun cor)
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_bend_fun(raq, vex, sab);
    }
  }

/* cold
*/
  u2_noun
  _cqe_cold_fun(
                         u2_noun cus,
                         u2_noun sef,
                         u2_noun tub)
  {
    u2_noun vex = u2_cx_good(u2_cn_slam_on(u2k(sef), u2k(tub)));
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);

    if ( u2_no == u2du(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun quq_vex;
      u2_noun ret;

      u2_cx_cell(uq_vex, 0, &quq_vex);
      ret = u2nq(u2k(p_vex),
                         u2_nul,
                         u2k(cus),
                         u2k(quq_vex));

      u2z(vex);
      return ret;
    }
  }

  u2_noun
 u2_cwe_cold_fun(
                        u2_noun cor)
  {
    u2_noun van, cus, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &cus, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_cold_fun(cus, sef, tub);
    }
  }

/* cook
*/
  u2_noun
  _cqe_cook_fun(
                         u2_noun poq,
                         u2_noun sef,
                         u2_noun tub)
  {
    u2_noun vex = u2_cx_good(u2_cn_slam_on(u2k(sef), u2k(tub)));
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);

    if ( u2_no == u2du(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = u2_cx_good(u2_cn_slam_on(u2k(poq), u2k(puq_vex)));
      ret = u2nq(u2k(p_vex),
                         u2_nul,
                         wag,
                         u2k(quq_vex));

      u2z(vex);
      return ret;
    }
  }

  u2_noun
 u2_cwe_cook_fun(
                        u2_noun cor)
  {
    u2_noun van, poq, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &poq, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_cook_fun(poq, sef, tub);
    }
  }

/* comp
*/
  u2_noun
  _cqe_comp_fun(
                         u2_noun raq,
                         u2_noun vex,
                         u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    } else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u2_no == u2du(q_yit) ) {
        ret = u2nc(yur, u2k(q_yit));
      }
      else {
        u2_noun uq_yit = u2t(q_yit);
        u2_noun puq_yit, quq_yit;

        u2_cx_cell(uq_yit, &puq_yit, &quq_yit);
        ret = u2nq(yur,
                           u2_nul,
                           u2_cx_good
                            (
                             u2_cn_slam_on
                               (u2k(raq),
                                       u2nc(u2k(puq_vex),
                                                    u2k(puq_yit)))),
                           u2k(quq_yit));
      }
      u2z(yit);
      return ret;
    }
  }

  u2_noun
 u2_cwe_comp_fun(
                        u2_noun cor)
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_comp_fun(raq, vex, sab);
    }
  }

/* easy
*/
  u2_noun
  _cqe_easy_fun(
                         u2_noun huf,
                         u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);
    return u2nq(u2k(p_tub),
                        u2_nul,
                        u2k(huf),
                        u2k(tub));
  }
  u2_noun
 u2_cwe_easy_fun(
                        u2_noun cor)
  {
    u2_noun van, huf, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (huf = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_easy_fun(huf, tub);
    }
  }

/* glue
*/
  u2_noun
  _cqe_glue_fun(
                         u2_noun bus,
                         u2_noun vex,
                         u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(bus), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u2_no == u2du(q_yit) ) {
        ret = u2nc(yur, u2_nul);
      }
      else {
        u2_noun uq_yit = u2t(q_yit);
        u2_noun puq_yit, quq_yit;
        u2_noun wam, p_wam, q_wam, goy;

        u2_cx_cell(uq_yit, &puq_yit, &quq_yit);
        wam = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_yit)));

        u2_cx_cell(wam, &p_wam, &q_wam);
        goy = _last(yur, p_wam);
        u2z(yur);

        if ( u2_no == u2du(q_wam) ) {
          ret = u2nc(goy, u2_nul);
        } else {
          u2_noun uq_wam = u2t(q_wam);
          u2_noun puq_wam, quq_wam;

          u2_cx_cell(uq_wam, &puq_wam, &quq_wam);
          ret = u2nq(goy,
                             u2_nul,
                             u2nc(u2k(puq_vex),
                                          u2k(puq_wam)),
                             u2k(quq_wam));
        }
        u2z(wam);
      }
      u2z(yit);
      return ret;
    }
  }

  u2_noun
 u2_cwe_glue_fun(
                        u2_noun cor)
  {
    u2_noun van, bus, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (bus = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_glue_fun(bus, vex, sab);
    }
  }

/* here
*/
  u2_noun
  _cqe_here_fun(
                         u2_noun hez,
                         u2_noun sef,
                         u2_noun tub)
  {
    u2_noun vex = u2_cx_good(u2_cn_slam_on(u2k(sef), u2k(tub)));
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);

    if ( u2_no == u2du(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun p_tub, q_tub;
      u2_noun puq_vex, quq_vex, pquq_vex;
      u2_noun gud, wag;
      u2_noun ret;

      u2_cx_cell(tub, &p_tub, &q_tub);
      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      u2_cx_cell(quq_vex, &pquq_vex, 0);
      gud = u2nc(
                  u2nc(u2k(p_tub),
                               u2k(pquq_vex)),
                  u2k(puq_vex));

      wag = u2_cx_good(u2_cn_slam_on(u2k(hez), gud));
      ret = u2nq(u2k(p_vex),
                         u2_nul,
                         wag,
                         u2k(quq_vex));

      u2z(vex);
      return ret;
    }
  }

  u2_noun
 u2_cwe_here_fun(
                        u2_noun cor)
  {
    u2_noun van, hez, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &hez, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_here_fun(hez, sef, tub);
    }
  }

/* just
*/
  u2_noun
  _cqe_just_fun(
                         u2_noun daf,
                         u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);

    if ( u2_no == u2du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u2_noun iq_tub = u2h(q_tub);

      if ( u2_yes == u2_cr_sing(daf, iq_tub) ) {
        return _next(tub);
      }
      else return _fail(tub);
    }
  }
  u2_noun
 u2_cwe_just_fun(
                        u2_noun cor)
  {
    u2_noun van, daf, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (daf = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_just_fun(daf, tub);
    }
  }

/* mask
*/
  u2_noun
  _cqe_mask_fun(
                         u2_noun bud,
                         u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);

    if ( u2_no == u2du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u2_noun iq_tub = u2h(q_tub);

      while ( u2_yes == u2du(bud) ) {
        if ( u2_yes == u2_cr_sing(u2h(bud), iq_tub) ) {
          return _next(tub);
        }
        bud = u2t(bud);
      }
      return _fail(tub);
    }
  }
  u2_noun
 u2_cwe_mask_fun(
                        u2_noun cor)
  {
    u2_noun van, bud, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (bud = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_mask_fun(bud, tub);
    }
  }
/* pfix
*/
  u2_noun
  _cqe_pfix(
                    u2_noun vex,
                    u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, p_yit, q_yit;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      ret = u2nc
        (
         _last(p_vex, p_yit),
         u2k(q_yit));

      u2z(yit);
      return ret;
    }
  }
  u2_noun
  u2_cwe_pfix(
                   u2_noun cor)
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return _cqe_pfix(vex, sab);
    }
  }

/* plug
*/
  u2_noun
  _cqe_plug(
                    u2_noun vex,
                    u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u2_no == u2du(q_yit) ) {
        ret = u2nc(yur, u2k(q_yit));
      }
      else {
        u2_noun uq_yit = u2t(q_yit);
        u2_noun puq_yit, quq_yit;

        u2_cx_cell(uq_yit, &puq_yit, &quq_yit);
        ret = u2nq(yur,
                           u2_nul,
                           u2nc(u2k(puq_vex),
                                        u2k(puq_yit)),
                           u2k(quq_yit));
      }
      u2z(yit);
      return ret;
    }
  }
  u2_noun
  u2_cwe_plug(
                   u2_noun cor)
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return _cqe_plug(vex, sab);
    }
  }

/* pose
*/
  u2_noun
  u2_cqe_pose(u2_noun vex,
                    u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_yes == u2du(q_vex) ) {
      return u2k(vex);
    } else {
      u2_noun roq = u2_cx_good(u2_cn_kick_on(u2k(sab)));
      u2_noun p_roq, q_roq;
      u2_noun ret;

      u2_cx_cell(roq, &p_roq, &q_roq);
      ret = u2nc
        (
         _last(p_vex, p_roq),
         u2k(q_roq));

      u2z(roq);
      return ret;
    }
  }
  u2_noun
  u2_cwe_pose(
                   u2_noun cor)
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqe_pose(vex, sab);
    }
  }

/* sfix
*/
  u2_noun
  _cqe_sfix(u2_noun vex, u2_noun sab)
  {
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      return u2k(vex);
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, p_yit, q_yit, yur;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u2_cx_good(u2_cn_slam_on(u2k(sab), u2k(quq_vex)));

      u2_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u2_no == u2du(q_yit) ) {
        ret = u2nc(yur, u2_nul);
      }
      else {
        u2_noun uq_yit = u2t(q_yit);
        u2_noun puq_yit, quq_yit;

        u2_cx_cell(uq_yit, &puq_yit, &quq_yit);

        ret = u2nq(yur,
                           u2_nul,
                           u2k(puq_vex),
                           u2k(quq_yit));
      }
      u2z(yit);
      return ret;
    }
  }
  u2_noun
  u2_cwe_sfix(u2_noun cor)
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return _cqe_sfix(vex, sab);
    }
  }

/* shim
*/
  u2_noun
  _cqe_shim_fun(u2_noun zep,
                         u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);

    if ( u2_no == u2du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u2_noun p_zep, q_zep;
      u2_noun iq_tub = u2h(q_tub);

      u2_cx_cell(zep, &p_zep, &q_zep);
      if ( u2_so(u2_co_is_cat(p_zep)) &&
           u2_so(u2_co_is_cat(q_zep)) &&
           u2_so(u2_co_is_cat(iq_tub)) )
      {
        if ( (iq_tub >= p_zep) && (iq_tub <= q_zep) ) {
          return _next(tub);
        }
        else return _fail(tub);
      }
      else {
        return u2_cm_bail(c3__fail);
      }
    }
  }
  u2_noun
 u2_cwe_shim_fun(
                        u2_noun cor)
  {
    u2_noun van, zep, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (zep = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_shim_fun(zep, tub);
    }
  }

/* stag
*/
  u2_noun
  _cqe_stag_fun(
                         u2_noun gob,
                         u2_noun sef,
                         u2_noun tub)
  {
    u2_noun vex = u2_cx_good(u2_cn_slam_on(u2k(sef), u2k(tub)));
    u2_noun p_vex, q_vex;

    u2_cx_cell(vex, &p_vex, &q_vex);

    if ( u2_no == u2du(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag;
      u2_noun ret;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = u2nc(u2k(gob), u2k(puq_vex));
      ret = u2nq(u2k(p_vex),
                         u2_nul,
                         wag,
                         u2k(quq_vex));

      u2z(vex);
      return ret;
    }
  }

  u2_noun
 u2_cwe_stag_fun(
                        u2_noun cor)
  {
    u2_noun van, gob, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &gob, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_stag_fun(gob, sef, tub);
    }
  }

/* stew
*/
  static u2_bean
  _stew_wor(
            u2_noun ort,
            u2_noun wan)
  {
    if ( u2_ne(u2_co_is_cat(ort)) ) {
      return u2_cm_bail(c3__fail);
    }
    else {
      if ( u2_no == u2du(wan) ) {
        if ( u2_ne(u2_co_is_cat(wan)) ) {
          return u2_cm_bail(c3__fail);
        }
        else return (ort < wan) ? u2_yes : u2_no;
      }
      else {
        u2_noun h_wan = u2h(wan);

        if ( u2_ne(u2_co_is_cat(h_wan)) ) {
          return u2_cm_bail(c3__fail);
        }
        else return (ort < h_wan) ? u2_yes : u2_no;
      }
    }
  }

  u2_noun
  _cqe_stew_fun(
                         u2_noun hel,
                         u2_noun tub)
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);
    if ( u2_no == u2du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u2_noun iq_tub = u2h(q_tub);

      if ( u2_ne(u2_co_is_cat(iq_tub)) ) {
        return u2_cm_bail(c3__fail);
      }
      else while ( 1 ) {
        if ( u2_no == u2du(hel) ) {
          return _fail(tub);
        }
        else {
          u2_noun n_hel, l_hel, r_hel;
          u2_noun pn_hel, qn_hel;
          c3_t    bit_t;

          u2_cx_trel(hel, &n_hel, &l_hel, &r_hel);
          u2_cx_cell(n_hel, &pn_hel, &qn_hel);

          if ( (u2_no == u2du(pn_hel)) ) {
            bit_t = (iq_tub == pn_hel);
          }
          else {
            u2_noun hpn_hel = u2h(pn_hel);
            u2_noun tpn_hel = u2t(pn_hel);

            if ( u2_ne(u2_co_is_cat(hpn_hel)) ||
                 u2_ne(u2_co_is_cat(tpn_hel)) ) {
              return _fail(tub);
            }
            else bit_t = (iq_tub >= hpn_hel) && (iq_tub <= tpn_hel);
          }

          if ( bit_t ) {
            return u2_cx_good
              (u2_cn_slam_on(u2k(qn_hel), u2k(tub)));
          } else {
            if ( u2_yes == _stew_wor(iq_tub, pn_hel) ) {
              hel = l_hel;
            }
            else hel = r_hel;
          }
        }
      }
    }
  }
  u2_noun
 u2_cwe_stew_fun(
                        u2_noun cor)
  {
    u2_noun con, hel, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &con, 0)) ||
         (u2_none == (hel = u2_cr_at(2, con))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_stew_fun(hel, tub);
    }
  }

/* stir
*/
  u2_noun
  _cqe_stir_fun(u2_noun rud,
                         u2_noun raq,
                         u2_noun fel,
                         u2_noun tub)
  {
    u2_noun vex = u2_cx_good(u2_cn_slam_on(u2k(fel), u2k(tub)));
    u2_noun p_vex, q_vex;
    u2_noun ret;

    u2_cx_cell(vex, &p_vex, &q_vex);
    if ( u2_no == u2du(q_vex) ) {
      ret = u2nq(u2k(p_vex),
                         u2_nul,
                         u2k(rud),
                         u2k(tub));
    }
    else {
      u2_noun uq_vex = u2t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag, p_wag, q_wag, uq_wag, puq_wag, quq_wag;

      u2_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = _cqe_stir_fun(rud, raq, fel, quq_vex);

      u2_cx_cell(wag, &p_wag, &q_wag);
      if ( u2_no == u2du(q_wag) ) {
        return u2_cm_bail(c3__fail);
      }
      uq_wag = u2t(q_wag);
      u2_cx_cell(uq_wag, &puq_wag, &quq_wag);

      ret = u2nq
        (_last(p_vex, p_wag),
                u2_nul,
                u2_cx_good(u2_cn_slam_on
                            (u2k(raq),
                                    u2nc(u2k(puq_vex),
                                                 u2k(puq_wag)))),
                u2k(quq_wag));
      u2z(wag);
    }
    u2z(vex);
    return ret;
  }

  u2_noun
  u2_cwe_stir_fun(u2_noun cor)
  {
    u2_noun van, rud, raq, fel, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &rud,
                                u2_cv_sam_6, &raq,
                                u2_cv_sam_7, &fel,
                                0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqe_stir_fun(rud, raq, fel, tub);
    }
  }
