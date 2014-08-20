/* j/5/parse.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun                                                  //  produce
  _slip(
        u2_noun weq,                                              //  retain
        u2_noun naz)                                              //  retain
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

  static u2_noun                                                  //  produce
  _fail(
        u2_noun tub)                                              //  retain
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);
    return u2nc(u2k(p_tub), u2_nul);
  }

  static u2_noun                                                  //  produce
  _last(
        u2_noun zyc,                                              //  retain
        u2_noun naz)                                              //  retain
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

  static u2_noun                                                  //  produce
  _next(
        u2_noun tub)                                              //  retain
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
  u2_noun                                                         //  produce
  j2_mcx(Pt5, bend, fun)(u2_noun raq,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, bend, fun)(u2_noun cor)                              //  retain
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, bend, fun)(raq, vex, sab);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, bend, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, bend, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* cold
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, cold, fun)(
                         u2_noun cus,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, cold, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, cus, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &cus, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, cold, fun)(cus, sef, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, cold, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, cold, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* cook
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, cook, fun)(
                         u2_noun poq,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, cook, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, poq, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &poq, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, cook, fun)(poq, sef, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, cook, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, cook, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* comp
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, comp, fun)(
                         u2_noun raq,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, comp, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, comp, fun)(raq, vex, sab);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, comp, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, comp, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* easy
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, easy, fun)(
                         u2_noun huf,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_cx_cell(tub, &p_tub, &q_tub);
    return u2nq(u2k(p_tub),
                        u2_nul,
                        u2k(huf),
                        u2k(tub));
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, easy, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, huf, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (huf = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, easy, fun)(huf, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, easy, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, easy, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* glue
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, glue, fun)(
                         u2_noun bus,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, glue, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, bus, vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (bus = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, glue, fun)(bus, vex, sab);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, glue, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, glue, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* here
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, here, fun)(
                         u2_noun hez,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, here, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, hez, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &hez, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, here, fun)(hez, sef, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, here, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, here, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* just
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, just, fun)(
                         u2_noun daf,                             //  retain
                         u2_noun tub)                             //  retain
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
  u2_noun                                                         //  produce
  j2_mc(Pt5, just, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, daf, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (daf = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, just, fun)(daf, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, just, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, just, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* mask
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, mask, fun)(
                         u2_noun bud,                             //  retain
                         u2_noun tub)                             //  retain
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
  u2_noun                                                         //  produce
  j2_mc(Pt5, mask, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, bud, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (bud = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, mask, fun)(bud, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, mask, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, mask, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* pfix
*/
  u2_noun                                                         //  produce
  j2_mbx(Pt5, pfix)(
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
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
  u2_noun                                                         //  produce
  j2_mb(Pt5, pfix)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbx(Pt5, pfix)(vex, sab);
    }
  }
  u2_ho_jet
  j2_mbj(Pt5, pfix)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, pfix), Tier5_b, u2_none, u2_none },
    { }
  };

/* plug
*/
  u2_noun                                                         //  produce
  j2_mbx(Pt5, plug)(
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
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
  u2_noun                                                         //  produce
  j2_mb(Pt5, plug)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbx(Pt5, plug)(vex, sab);
    }
  }
  u2_ho_jet
  j2_mbj(Pt5, plug)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, plug), Tier5_b, u2_none, u2_none },
    { }
  };

/* pose
*/
  u2_noun                                                         //  produce
  j2_mbx(Pt5, pose)(u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
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
  u2_noun                                                         //  produce
  j2_mb(Pt5, pose)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbx(Pt5, pose)(vex, sab);
    }
  }
  u2_ho_jet
  j2_mbj(Pt5, pose)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, pose), Tier5_b, u2_none, u2_none },
    { }
  };

/* sfix
*/
  u2_noun                                                         //  produce
  j2_mbx(Pt5, sfix)(
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
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
  u2_noun                                                         //  produce
  j2_mb(Pt5, sfix)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbx(Pt5, sfix)(vex, sab);
    }
  }
  u2_ho_jet
  j2_mbj(Pt5, sfix)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, sfix), Tier5_b, u2_none, u2_none },
    { }
  };

/* shim
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, shim, fun)(u2_noun zep,                             //  retain
                         u2_noun tub)                             //  retain
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
  u2_noun                                                         //  produce
  j2_mc(Pt5, shim, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, zep, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (zep = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, shim, fun)(zep, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, shim, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, shim, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* stag
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, stag, fun)(
                         u2_noun gob,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
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

  u2_noun                                                         //  produce
  j2_mc(Pt5, stag, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun van, gob, sef, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_cr_mean(van, u2_cv_sam_2, &gob, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, stag, fun)(gob, sef, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, stag, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, stag, fun), Tier5_b, u2_none, u2_none },
    { }
  };

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

  u2_noun                                                         //  produce
  j2_mcx(Pt5, stew, fun)(
                         u2_noun hel,                             //  retain
                         u2_noun tub)                             //  retain
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
  u2_noun                                                         //  produce
  j2_mc(Pt5, stew, fun)(
                        u2_noun cor)                              //  retain
  {
    u2_noun con, hel, tub;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &tub, u2_cv_con, &con, 0)) ||
         (u2_none == (hel = u2_cr_at(2, con))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt5, stew, fun)(hel, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, stew, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, stew, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* stir
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, stir, fun)(u2_noun rud,                             //  retain
                         u2_noun raq,                             //  retain
                         u2_noun fel,                             //  retain
                         u2_noun tub)                             //  retain
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
      wag = j2_mcx(Pt5, stir, fun)(rud, raq, fel, quq_vex);

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

  u2_noun                                                         //  produce
  j2_mc(Pt5, stir, fun)(
                        u2_noun cor)                              //  retain
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
      return j2_mcx(Pt5, stir, fun)(rud, raq, fel, tub);
    }
  }
  u2_ho_jet
  j2_mcj(Pt5, stir, fun)[] = {
    { ".2", c3__hevy, j2_mc(Pt5, stir, fun), Tier5_b, u2_none, u2_none },
    { }
  };

/* structures
*/
  /* bend
  */
    u2_ho_driver
    j2_mbd(Pt5, bend)[] = {
      { j2_sc(Pt5, bend, fun), j2_mcj(Pt5, bend, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, bend) = { j2_sb(Pt5, bend), 0, j2_mbd(Pt5, bend), 0, u2_none };

  /* cold
  */
    u2_ho_driver
    j2_mbd(Pt5, cold)[] = {
      { j2_sc(Pt5, cold, fun), j2_mcj(Pt5, cold, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, cold) = { j2_sb(Pt5, cold), 0, j2_mbd(Pt5, cold), 0, u2_none };

  /* comp
  */
    u2_ho_driver
    j2_mbd(Pt5, comp)[] = {
      { j2_sc(Pt5, comp, fun), j2_mcj(Pt5, comp, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, comp) = { j2_sb(Pt5, comp), 0, j2_mbd(Pt5, comp), 0, u2_none };

  /* cook
  */
    u2_ho_driver
    j2_mbd(Pt5, cook)[] = {
      { j2_sc(Pt5, cook, fun), j2_mcj(Pt5, cook, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, cook) = { j2_sb(Pt5, cook), 0, j2_mbd(Pt5, cook), 0, u2_none };

  /* easy
  */
    u2_ho_driver
    j2_mbd(Pt5, easy)[] = {
      { j2_sc(Pt5, easy, fun), j2_mcj(Pt5, easy, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, easy) = { j2_sb(Pt5, easy), 0, j2_mbd(Pt5, easy), 0, u2_none };

  /* glue
  */
    u2_ho_driver
    j2_mbd(Pt5, glue)[] = {
      { j2_sc(Pt5, glue, fun), j2_mcj(Pt5, glue, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, glue) = { j2_sb(Pt5, glue), 0, j2_mbd(Pt5, glue), 0, u2_none };

  /* here
  */
    u2_ho_driver
    j2_mbd(Pt5, here)[] = {
      { j2_sc(Pt5, here, fun), j2_mcj(Pt5, here, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, here) = { j2_sb(Pt5, here), 0, j2_mbd(Pt5, here), 0, u2_none };

  /* just
  */
    u2_ho_driver
    j2_mbd(Pt5, just)[] = {
      { j2_sc(Pt5, just, fun), j2_mcj(Pt5, just, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, just) = { j2_sb(Pt5, just), 0, j2_mbd(Pt5, just), 0, u2_none };

  /* mask
  */
    u2_ho_driver
    j2_mbd(Pt5, mask)[] = {
      { j2_sc(Pt5, mask, fun), j2_mcj(Pt5, mask, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, mask) = { j2_sb(Pt5, mask), 0, j2_mbd(Pt5, mask), 0, u2_none };

  /* shim
  */
    u2_ho_driver
    j2_mbd(Pt5, shim)[] = {
      { j2_sc(Pt5, shim, fun), j2_mcj(Pt5, shim, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, shim) = { j2_sb(Pt5, shim), 0, j2_mbd(Pt5, shim), 0, u2_none };

  /* stag
  */
    u2_ho_driver
    j2_mbd(Pt5, stag)[] = {
      { j2_sc(Pt5, stag, fun), j2_mcj(Pt5, stag, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, stag) = { j2_sb(Pt5, stag), 0, j2_mbd(Pt5, stag), 0, u2_none };

  /* stew
  */
    u2_ho_driver
    j2_mbd(Pt5, stew)[] = {
      { j2_sc(Pt5, stew, fun), j2_mcj(Pt5, stew, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, stew) = { j2_sb(Pt5, stew), 0, j2_mbd(Pt5, stew), 0, u2_none };

  /* stir
  */
    u2_ho_driver
    j2_mbd(Pt5, stir)[] = {
      { j2_sc(Pt5, stir, fun), j2_mcj(Pt5, stir, fun), 0, 0, u2_none },
      { }
    };

    u2_ho_driver
    j2_db(Pt5, stir) = { j2_sb(Pt5, stir), 0, j2_mbd(Pt5, stir), 0, u2_none };
