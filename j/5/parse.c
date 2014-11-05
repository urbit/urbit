/* j/5/parse.c
**
** This file is in the public domain.
*/
#include "all.h"


  static u3_noun
  _slip(
        u3_noun weq,
        u3_noun naz)
  {
    u3_noun p_naz, q_naz;

    u3_cx_cell(naz, &p_naz, &q_naz);
    if ( 10 == weq ) {
      return u3nc(u3_cx_good(
                                     u3_ci_vint(u3k(p_naz))),
                          1);
    } else {
      return u3nc(u3k(p_naz),
                          u3_cx_good(
                                     u3_ci_vint(u3k(q_naz))));
    }
  }

  static u3_noun
  _fail(
        u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);
    return u3nc(u3k(p_tub), u3_nul);
  }

  static u3_noun
  _last(
        u3_noun zyc,
        u3_noun naz)
  {
    u3_noun p_zyc, q_zyc, p_naz, q_naz;

    u3_cx_cell(zyc, &p_zyc, &q_zyc);
    u3_cx_cell(naz, &p_naz, &q_naz);

    if ( u3_ne(u3_ca_is_cat(p_zyc)) || u3_ne(u3_ca_is_cat(q_zyc)) ||
         u3_ne(u3_ca_is_cat(p_naz)) || u3_ne(u3_ca_is_cat(q_naz)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      if ( p_zyc == p_naz ) {
        return (q_zyc > q_naz) ? u3k(zyc) : u3k(naz);
      }
      else {
        return (p_zyc > p_naz) ? u3k(zyc) : u3k(naz);
      }
    }
  }

  static u3_noun
  _next(
        u3_noun tub)
  {
    u3_noun p_tub, q_tub;
    u3_noun zac;

    u3_cx_cell(tub, &p_tub, &q_tub);
    if ( u3_no == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);
      u3_noun tq_tub = u3t(q_tub);

      zac = _slip(iq_tub, p_tub);

      return u3nc
        (zac,
                u3nq(u3_nul,
                             u3k(iq_tub),
                             u3k(zac),
                             u3k(tq_tub)));
    }
  }

/* bend
*/
  u3_noun
  _cqe_bend_fun(u3_noun raq,
                         u3_noun vex,
                         u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u3_no == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_vex));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;
        u3_noun vux;

        u3_cx_cell(uq_yit, &puq_yit, &quq_yit);

        vux = u3_cx_good
                    (u3_cn_slam_on
                       (u3k(raq),
                               u3nc(u3k(puq_vex),
                                            u3k(puq_yit))));
        if ( u3_nul == vux ) {
          ret = u3nc(yur, u3k(q_vex));
        }
        else {
          ret = u3nq(yur,
                             u3_nul,
                             u3k(u3t(vux)),
                             u3k(quq_yit));
          u3z(vux);
        }
      }
      u3z(yit);
      return ret;
    }
  }

  u3_noun
 u3_cwe_bend_fun(u3_noun cor)
  {
    u3_noun van, raq, vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex,
                                u3_cv_sam_3, &sab,
                                u3_cv_con, &van, 0)) ||
         (u3_none == (raq = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_bend_fun(raq, vex, sab);
    }
  }

/* cold
*/
  u3_noun
  _cqe_cold_fun(
                         u3_noun cus,
                         u3_noun sef,
                         u3_noun tub)
  {
    u3_noun vex = u3_cx_good(u3_cn_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);

    if ( u3_no == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun quq_vex;
      u3_noun ret;

      u3_cx_cell(uq_vex, 0, &quq_vex);
      ret = u3nq(u3k(p_vex),
                         u3_nul,
                         u3k(cus),
                         u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3_cwe_cold_fun(
                        u3_noun cor)
  {
    u3_noun van, cus, sef, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_no == u3_cr_mean(van, u3_cv_sam_2, &cus, u3_cv_sam_3, &sef, 0)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_cold_fun(cus, sef, tub);
    }
  }

/* cook
*/
  u3_noun
  _cqe_cook_fun(
                         u3_noun poq,
                         u3_noun sef,
                         u3_noun tub)
  {
    u3_noun vex = u3_cx_good(u3_cn_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);

    if ( u3_no == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun wag;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = u3_cx_good(u3_cn_slam_on(u3k(poq), u3k(puq_vex)));
      ret = u3nq(u3k(p_vex),
                         u3_nul,
                         wag,
                         u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3_cwe_cook_fun(
                        u3_noun cor)
  {
    u3_noun van, poq, sef, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_no == u3_cr_mean(van, u3_cv_sam_2, &poq, u3_cv_sam_3, &sef, 0)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_cook_fun(poq, sef, tub);
    }
  }

/* comp
*/
  u3_noun
  _cqe_comp_fun(
                         u3_noun raq,
                         u3_noun vex,
                         u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u3_no == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_yit));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3_cx_cell(uq_yit, &puq_yit, &quq_yit);
        ret = u3nq(yur,
                           u3_nul,
                           u3_cx_good
                            (
                             u3_cn_slam_on
                               (u3k(raq),
                                       u3nc(u3k(puq_vex),
                                                    u3k(puq_yit)))),
                           u3k(quq_yit));
      }
      u3z(yit);
      return ret;
    }
  }

  u3_noun
 u3_cwe_comp_fun(
                        u3_noun cor)
  {
    u3_noun van, raq, vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex,
                                u3_cv_sam_3, &sab,
                                u3_cv_con, &van, 0)) ||
         (u3_none == (raq = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_comp_fun(raq, vex, sab);
    }
  }

/* easy
*/
  u3_noun
  _cqe_easy_fun(
                         u3_noun huf,
                         u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);
    return u3nq(u3k(p_tub),
                        u3_nul,
                        u3k(huf),
                        u3k(tub));
  }
  u3_noun
 u3_cwe_easy_fun(
                        u3_noun cor)
  {
    u3_noun van, huf, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_none == (huf = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_easy_fun(huf, tub);
    }
  }

/* glue
*/
  u3_noun
  _cqe_glue_fun(
                         u3_noun bus,
                         u3_noun vex,
                         u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(bus), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u3_no == u3du(q_yit) ) {
        ret = u3nc(yur, u3_nul);
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;
        u3_noun wam, p_wam, q_wam, goy;

        u3_cx_cell(uq_yit, &puq_yit, &quq_yit);
        wam = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_yit)));

        u3_cx_cell(wam, &p_wam, &q_wam);
        goy = _last(yur, p_wam);
        u3z(yur);

        if ( u3_no == u3du(q_wam) ) {
          ret = u3nc(goy, u3_nul);
        } else {
          u3_noun uq_wam = u3t(q_wam);
          u3_noun puq_wam, quq_wam;

          u3_cx_cell(uq_wam, &puq_wam, &quq_wam);
          ret = u3nq(goy,
                             u3_nul,
                             u3nc(u3k(puq_vex),
                                          u3k(puq_wam)),
                             u3k(quq_wam));
        }
        u3z(wam);
      }
      u3z(yit);
      return ret;
    }
  }

  u3_noun
 u3_cwe_glue_fun(
                        u3_noun cor)
  {
    u3_noun van, bus, vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex,
                                u3_cv_sam_3, &sab,
                                u3_cv_con, &van, 0)) ||
         (u3_none == (bus = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_glue_fun(bus, vex, sab);
    }
  }

/* here
*/
  u3_noun
  _cqe_here_fun(
                         u3_noun hez,
                         u3_noun sef,
                         u3_noun tub)
  {
    u3_noun vex = u3_cx_good(u3_cn_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);

    if ( u3_no == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun p_tub, q_tub;
      u3_noun puq_vex, quq_vex, pquq_vex;
      u3_noun gud, wag;
      u3_noun ret;

      u3_cx_cell(tub, &p_tub, &q_tub);
      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      u3_cx_cell(quq_vex, &pquq_vex, 0);
      gud = u3nc(
                  u3nc(u3k(p_tub),
                               u3k(pquq_vex)),
                  u3k(puq_vex));

      wag = u3_cx_good(u3_cn_slam_on(u3k(hez), gud));
      ret = u3nq(u3k(p_vex),
                         u3_nul,
                         wag,
                         u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3_cwe_here_fun(
                        u3_noun cor)
  {
    u3_noun van, hez, sef, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_no == u3_cr_mean(van, u3_cv_sam_2, &hez, u3_cv_sam_3, &sef, 0)) )
    {
      return u3_cm_bail(c3__fail);
    } 
    else {
      return _cqe_here_fun(hez, sef, tub);
    }
  }

/* just
*/
  u3_noun
  _cqe_just_fun(
                         u3_noun daf,
                         u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);

    if ( u3_no == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      if ( u3_yes == u3_cr_sing(daf, iq_tub) ) {
        return _next(tub);
      }
      else return _fail(tub);
    }
  }
  u3_noun
 u3_cwe_just_fun(
                        u3_noun cor)
  {
    u3_noun van, daf, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_none == (daf = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_just_fun(daf, tub);
    }
  }

/* mask
*/
  u3_noun
  _cqe_mask_fun(
                         u3_noun bud,
                         u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);

    if ( u3_no == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      while ( u3_yes == u3du(bud) ) {
        if ( u3_yes == u3_cr_sing(u3h(bud), iq_tub) ) {
          return _next(tub);
        }
        bud = u3t(bud);
      }
      return _fail(tub);
    }
  }
  u3_noun
 u3_cwe_mask_fun(
                        u3_noun cor)
  {
    u3_noun van, bud, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_none == (bud = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_mask_fun(bud, tub);
    }
  }
/* pfix
*/
  u3_noun
  _cqe_pfix(
                    u3_noun vex,
                    u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, p_yit, q_yit;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      ret = u3nc
        (
         _last(p_vex, p_yit),
         u3k(q_yit));

      u3z(yit);
      return ret;
    }
  }
  u3_noun
  u3_cwe_pfix(
                   u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex, u3_cv_sam_3, &sab, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return _cqe_pfix(vex, sab);
    }
  }

/* plug
*/
  u3_noun
  _cqe_plug(
                    u3_noun vex,
                    u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u3_no == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_yit));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3_cx_cell(uq_yit, &puq_yit, &quq_yit);
        ret = u3nq(yur,
                           u3_nul,
                           u3nc(u3k(puq_vex),
                                        u3k(puq_yit)),
                           u3k(quq_yit));
      }
      u3z(yit);
      return ret;
    }
  }
  u3_noun
  u3_cwe_plug(
                   u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex, u3_cv_sam_3, &sab, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return _cqe_plug(vex, sab);
    }
  }

/* pose
*/
  u3_noun
  u3_cqe_pose(u3_noun vex,
                    u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_yes == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun roq = u3_cx_good(u3_cn_kick_on(u3k(sab)));
      u3_noun p_roq, q_roq;
      u3_noun ret;

      u3_cx_cell(roq, &p_roq, &q_roq);
      ret = u3nc
        (
         _last(p_vex, p_roq),
         u3k(q_roq));

      u3z(roq);
      return ret;
    }
  }
  u3_noun
  u3_cwe_pose(
                   u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex, u3_cv_sam_3, &sab, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqe_pose(vex, sab);
    }
  }

/* sfix
*/
  u3_noun
  _cqe_sfix(u3_noun vex, u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, p_yit, q_yit, yur;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3_cx_good(u3_cn_slam_on(u3k(sab), u3k(quq_vex)));

      u3_cx_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( u3_no == u3du(q_yit) ) {
        ret = u3nc(yur, u3_nul);
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3_cx_cell(uq_yit, &puq_yit, &quq_yit);

        ret = u3nq(yur,
                           u3_nul,
                           u3k(puq_vex),
                           u3k(quq_yit));
      }
      u3z(yit);
      return ret;
    }
  }
  u3_noun
  u3_cwe_sfix(u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &vex, u3_cv_sam_3, &sab, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return _cqe_sfix(vex, sab);
    }
  }

/* shim
*/
  u3_noun
  _cqe_shim_fun(u3_noun zep,
                         u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);

    if ( u3_no == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun p_zep, q_zep;
      u3_noun iq_tub = u3h(q_tub);

      u3_cx_cell(zep, &p_zep, &q_zep);
      if ( u3_so(u3_ca_is_cat(p_zep)) &&
           u3_so(u3_ca_is_cat(q_zep)) &&
           u3_so(u3_ca_is_cat(iq_tub)) )
      {
        if ( (iq_tub >= p_zep) && (iq_tub <= q_zep) ) {
          return _next(tub);
        }
        else return _fail(tub);
      }
      else {
        return u3_cm_bail(c3__fail);
      }
    }
  }
  u3_noun
 u3_cwe_shim_fun(
                        u3_noun cor)
  {
    u3_noun van, zep, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_none == (zep = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_shim_fun(zep, tub);
    }
  }

/* stag
*/
  u3_noun
  _cqe_stag_fun(
                         u3_noun gob,
                         u3_noun sef,
                         u3_noun tub)
  {
    u3_noun vex = u3_cx_good(u3_cn_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3_cx_cell(vex, &p_vex, &q_vex);

    if ( u3_no == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun wag;
      u3_noun ret;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = u3nc(u3k(gob), u3k(puq_vex));
      ret = u3nq(u3k(p_vex),
                         u3_nul,
                         wag,
                         u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3_cwe_stag_fun(
                        u3_noun cor)
  {
    u3_noun van, gob, sef, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_no == u3_cr_mean(van, u3_cv_sam_2, &gob, u3_cv_sam_3, &sef, 0)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_stag_fun(gob, sef, tub);
    }
  }

/* stew
*/
  static u3_bean
  _stew_wor(
            u3_noun ort,
            u3_noun wan)
  {
    if ( u3_ne(u3_ca_is_cat(ort)) ) {
      return u3_cm_bail(c3__fail);
    }
    else {
      if ( u3_no == u3du(wan) ) {
        if ( u3_ne(u3_ca_is_cat(wan)) ) {
          return u3_cm_bail(c3__fail);
        }
        else return (ort < wan) ? u3_yes : u3_no;
      }
      else {
        u3_noun h_wan = u3h(wan);

        if ( u3_ne(u3_ca_is_cat(h_wan)) ) {
          return u3_cm_bail(c3__fail);
        }
        else return (ort < h_wan) ? u3_yes : u3_no;
      }
    }
  }

  u3_noun
  _cqe_stew_fun(
                         u3_noun hel,
                         u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3_cx_cell(tub, &p_tub, &q_tub);
    if ( u3_no == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      if ( u3_ne(u3_ca_is_cat(iq_tub)) ) {
        return u3_cm_bail(c3__fail);
      }
      else while ( 1 ) {
        if ( u3_no == u3du(hel) ) {
          return _fail(tub);
        }
        else {
          u3_noun n_hel, l_hel, r_hel;
          u3_noun pn_hel, qn_hel;
          c3_o    bit_o;

          u3_cx_trel(hel, &n_hel, &l_hel, &r_hel);
          u3_cx_cell(n_hel, &pn_hel, &qn_hel);

          if ( (u3_no == u3du(pn_hel)) ) {
            bit_o = u3_say((iq_tub == pn_hel));
          }
          else {
            u3_noun hpn_hel = u3h(pn_hel);
            u3_noun tpn_hel = u3t(pn_hel);

            if ( u3_ne(u3_ca_is_cat(hpn_hel)) ||
                 u3_ne(u3_ca_is_cat(tpn_hel)) ) {
              return _fail(tub);
            }
            else bit_o = u3_say((iq_tub >= hpn_hel) && (iq_tub <= tpn_hel));
          }

          if ( u3_yes == bit_o ) {
            return u3_cx_good
              (u3_cn_slam_on(u3k(qn_hel), u3k(tub)));
          } else {
            if ( u3_yes == _stew_wor(iq_tub, pn_hel) ) {
              hel = l_hel;
            }
            else hel = r_hel;
          }
        }
      }
    }
  }
 u3_noun
 u3_cwe_stew_fun(
                        u3_noun cor)
  {
    u3_noun con, hel, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &con, 0)) ||
         (u3_none == (hel = u3_cr_at(2, con))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_stew_fun(hel, tub);
    }
  }

/* stir
*/
  u3_noun
  _cqe_stir_fun(u3_noun rud,
                         u3_noun raq,
                         u3_noun fel,
                         u3_noun tub)
  {
    u3_noun vex = u3_cx_good(u3_cn_slam_on(u3k(fel), u3k(tub)));
    u3_noun p_vex, q_vex;
    u3_noun ret;

    u3_cx_cell(vex, &p_vex, &q_vex);
    if ( u3_no == u3du(q_vex) ) {
      ret = u3nq(u3k(p_vex),
                         u3_nul,
                         u3k(rud),
                         u3k(tub));
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun wag, p_wag, q_wag, uq_wag, puq_wag, quq_wag;

      u3_cx_cell(uq_vex, &puq_vex, &quq_vex);
      wag = _cqe_stir_fun(rud, raq, fel, quq_vex);

      u3_cx_cell(wag, &p_wag, &q_wag);
      if ( u3_no == u3du(q_wag) ) {
        return u3_cm_bail(c3__fail);
      }
      uq_wag = u3t(q_wag);
      u3_cx_cell(uq_wag, &puq_wag, &quq_wag);

      ret = u3nq
        (_last(p_vex, p_wag),
                u3_nul,
                u3_cx_good(u3_cn_slam_on
                            (u3k(raq),
                                    u3nc(u3k(puq_vex),
                                                 u3k(puq_wag)))),
                u3k(quq_wag));
      u3z(wag);
    }
    u3z(vex);
    return ret;
  }

  u3_noun
  u3_cwe_stir_fun(u3_noun cor)
  {
    u3_noun van, rud, raq, fel, tub;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &tub, u3_cv_con, &van, 0)) ||
         (u3_no == u3_cr_mean(van, u3_cv_sam_2, &rud,
                                u3_cv_sam_6, &raq,
                                u3_cv_sam_7, &fel,
                                0)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqe_stir_fun(rud, raq, fel, tub);
    }
  }
