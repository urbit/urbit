/* j/5/parse.c
**
*/
#include "all.h"


  static u3_noun
  _slip(u3_noun weq,
        u3_noun naz)
  {
    u3_noun p_naz, q_naz;

    u3x_cell(naz, &p_naz, &q_naz);
    if ( 10 == weq ) {
      return u3nc(u3x_good(u3i_vint(u3k(p_naz))),
                           1);
    } else {
      return u3nc(u3k(p_naz),
                  u3x_good(u3i_vint(u3k(q_naz))));
    }
  }

  static u3_noun
  _fail(u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);
    return u3nc(u3k(p_tub), u3_nul);
  }

  static u3_noun
  _last(u3_noun zyc,
        u3_noun naz)
  {
    u3_noun p_zyc, q_zyc, p_naz, q_naz;

    u3x_cell(zyc, &p_zyc, &q_zyc);
    u3x_cell(naz, &p_naz, &q_naz);

    if ( !_(u3a_is_cat(p_zyc)) || !_(u3a_is_cat(q_zyc)) ||
         !_(u3a_is_cat(p_naz)) || !_(u3a_is_cat(q_naz)) )
    {
      return u3m_bail(c3__fail);
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
  _last_k(u3_noun zyc, u3_noun naz)
  {
    u3_noun pro = _last(zyc, naz);
    u3z(zyc); u3z(naz);
    return pro;
  }

  static u3_noun
  _next(u3_noun tub)
  {
    u3_noun p_tub, q_tub;
    u3_noun zac;

    u3x_cell(tub, &p_tub, &q_tub);
    if ( c3n == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);
      u3_noun tq_tub = u3t(q_tub);

      zac = _slip(iq_tub, p_tub);

      return u3nc(zac,
                  u3nq(u3_nul,
                       u3k(iq_tub),
                       u3k(zac),
                       u3k(tq_tub)));
    }
  }

/* bend
*/
  static u3_noun
  _cqe_bend_fun(u3_noun raq,
                u3_noun vex,
                u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_vex));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;
        u3_noun vux;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);

        vux = u3x_good(u3n_slam_on(u3k(raq),
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
 u3we_bend_fun(u3_noun cor)
  {
    u3_noun van, raq, vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex,
                               u3x_sam_3, &sab,
                               u3x_con, &van, 0)) ||
         (u3_none == (raq = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_bend_fun(raq, vex, sab);
    }
  }

/* cold
*/
  static u3_noun
  _cqe_cold_fun(u3_noun cus,
                u3_noun sef,
                u3_noun tub)
  {
    u3_noun vex = u3x_good(u3n_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);

    if ( c3n == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun quq_vex;
      u3_noun ret;

      u3x_cell(uq_vex, 0, &quq_vex);
      ret = u3nq(u3k(p_vex),
                 u3_nul,
                 u3k(cus),
                 u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3we_cold_fun(u3_noun cor)
  {
    u3_noun van, cus, sef, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (c3n == u3r_mean(van, u3x_sam_2, &cus, u3x_sam_3, &sef, 0)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_cold_fun(cus, sef, tub);
    }
  }

/* cook
*/
  static u3_noun
  _cqe_cook_fun(u3_noun poq,
                u3_noun sef,
                u3_noun tub)
  {
    u3_noun vex = u3x_good(u3n_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);

    if ( c3n == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun wag;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      wag = u3x_good(u3n_slam_on(u3k(poq), u3k(puq_vex)));
      ret = u3nq(u3k(p_vex),
                 u3_nul,
                 wag,
                 u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3we_cook_fun(u3_noun cor)
  {
    u3_noun van, poq, sef, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (c3n == u3r_mean(van, u3x_sam_2, &poq, u3x_sam_3, &sef, 0)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_cook_fun(poq, sef, tub);
    }
  }

/* comp
*/
  static u3_noun
  _cqe_comp_fun(u3_noun raq,
                u3_noun vex,
                u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_yit));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);
        ret = u3nq(yur,
                   u3_nul,
                   u3x_good(u3n_slam_on(u3k(raq),
                                        u3nc(u3k(puq_vex),
                                             u3k(puq_yit)))),
                   u3k(quq_yit));
      }
      u3z(yit);
      return ret;
    }
  }

  u3_noun
 u3we_comp_fun(u3_noun cor)
  {
    u3_noun van, raq, vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex,
                               u3x_sam_3, &sab,
                               u3x_con, &van, 0)) ||
         (u3_none == (raq = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_comp_fun(raq, vex, sab);
    }
  }

/* easy
*/
  static u3_noun
  _cqe_easy_fun(u3_noun huf,
                u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);
    return u3nq(u3k(p_tub),
                u3_nul,
                u3k(huf),
                u3k(tub));
  }

  u3_noun
  u3we_easy_fun(u3_noun cor)
  {
    u3_noun van, huf, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (u3_none == (huf = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_easy_fun(huf, tub);
    }
  }

/* glue
*/
  static u3_noun
  _cqe_glue_fun(u3_noun bus,
                u3_noun vex,
                u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(bus), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3_nul);
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;
        u3_noun wam, p_wam, q_wam, goy;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);
        wam = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_yit)));

        u3x_cell(wam, &p_wam, &q_wam);
        goy = _last(yur, p_wam);
        u3z(yur);

        if ( c3n == u3du(q_wam) ) {
          ret = u3nc(goy, u3_nul);
        } else {
          u3_noun uq_wam = u3t(q_wam);
          u3_noun puq_wam, quq_wam;

          u3x_cell(uq_wam, &puq_wam, &quq_wam);
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
 u3we_glue_fun(u3_noun cor)
  {
    u3_noun van, bus, vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex,
                                u3x_sam_3, &sab,
                                u3x_con, &van, 0)) ||
         (u3_none == (bus = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_glue_fun(bus, vex, sab);
    }
  }

/* here
*/
  static u3_noun
  _cqe_here_fun(u3_noun hez,
                u3_noun sef,
                u3_noun tub)
  {
    u3_noun vex = u3x_good(u3n_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);

    if ( c3n == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun p_tub, q_tub;
      u3_noun puq_vex, quq_vex, pquq_vex;
      u3_noun gud, wag;
      u3_noun ret;

      u3x_cell(tub, &p_tub, &q_tub);
      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      u3x_cell(quq_vex, &pquq_vex, 0);
      gud = u3nc(
                  u3nc(u3k(p_tub),
                               u3k(pquq_vex)),
                  u3k(puq_vex));

      wag = u3x_good(u3n_slam_on(u3k(hez), gud));
      ret = u3nq(u3k(p_vex),
                 u3_nul,
                 wag,
                 u3k(quq_vex));

      u3z(vex);
      return ret;
    }
  }

  u3_noun
 u3we_here_fun(u3_noun cor)
  {
    u3_noun van, hez, sef, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (c3n == u3r_mean(van, u3x_sam_2, &hez, u3x_sam_3, &sef, 0)) )
    {
      return u3m_bail(c3__fail);
    }
    else {
      return _cqe_here_fun(hez, sef, tub);
    }
  }

/* just
*/
  static u3_noun
  _cqe_just_fun(u3_noun daf,
                u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);

    if ( c3n == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      if ( c3y == u3r_sing(daf, iq_tub) ) {
        return _next(tub);
      }
      else return _fail(tub);
    }
  }
  u3_noun
 u3we_just_fun(u3_noun cor)
  {
    u3_noun van, daf, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (u3_none == (daf = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_just_fun(daf, tub);
    }
  }

/* mask
*/
  static u3_noun
  _cqe_mask_fun(u3_noun bud,
                u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);

    if ( c3n == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      while ( c3y == u3du(bud) ) {
        if ( c3y == u3r_sing(u3h(bud), iq_tub) ) {
          return _next(tub);
        }
        bud = u3t(bud);
      }
      return _fail(tub);
    }
  }
  u3_noun
 u3we_mask_fun(u3_noun cor)
  {
    u3_noun van, bud, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (u3_none == (bud = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_mask_fun(bud, tub);
    }
  }
/* pfix
*/
  static u3_noun
  _cqe_pfix(u3_noun vex,
            u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      ret = u3nc(_last(p_vex, p_yit),
                 u3k(q_yit));

      u3z(yit);
      return ret;
    }
  }
  u3_noun
  u3we_pfix(u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex, u3x_sam_3, &sab, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return _cqe_pfix(vex, sab);
    }
  }

/* plug
*/
  static u3_noun
  _cqe_plug(u3_noun vex,
            u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, yur;
      u3_noun p_yit, q_yit;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3k(q_yit));
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);
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
  u3we_plug(u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex, u3x_sam_3, &sab, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return _cqe_plug(vex, sab);
    }
  }

/* pose
*/
  u3_noun
  u3qe_pose(u3_noun vex,
            u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3y == u3du(q_vex) ) {
      return u3k(vex);
    } else {
      u3_noun roq = u3x_good(u3n_kick_on(u3k(sab)));
      u3_noun p_roq, q_roq;
      u3_noun ret;

      u3x_cell(roq, &p_roq, &q_roq);
      ret = u3nc(_last(p_vex, p_roq),
                 u3k(q_roq));

      u3z(roq);
      return ret;
    }
  }
  u3_noun
  u3we_pose(u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex, u3x_sam_3, &sab, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_pose(vex, sab);
    }
  }

/* sfix
*/
  static u3_noun
  _cqe_sfix(u3_noun vex,
            u3_noun sab)
  {
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);
    if ( c3n == u3du(q_vex) ) {
      return u3k(vex);
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun yit, p_yit, q_yit, yur;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
      yit = u3x_good(u3n_slam_on(u3k(sab), u3k(quq_vex)));

      u3x_cell(yit, &p_yit, &q_yit);
      yur = _last(p_vex, p_yit);

      if ( c3n == u3du(q_yit) ) {
        ret = u3nc(yur, u3_nul);
      }
      else {
        u3_noun uq_yit = u3t(q_yit);
        u3_noun puq_yit, quq_yit;

        u3x_cell(uq_yit, &puq_yit, &quq_yit);

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
  u3we_sfix(u3_noun cor)
  {
    u3_noun vex, sab;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &vex, u3x_sam_3, &sab, 0)) ) {
      return u3m_bail(c3__exit);
    } else {
      return _cqe_sfix(vex, sab);
    }
  }

/* shim
*/
  static u3_noun
  _cqe_shim_fun(u3_noun zep,
                u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);

    if ( c3n == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun p_zep, q_zep;
      u3_noun iq_tub = u3h(q_tub);

      u3x_cell(zep, &p_zep, &q_zep);
      if ( _(u3a_is_cat(p_zep)) &&
           _(u3a_is_cat(q_zep)) &&
           _(u3a_is_cat(iq_tub)) )
      {
        if ( (iq_tub >= p_zep) && (iq_tub <= q_zep) ) {
          return _next(tub);
        }
        else return _fail(tub);
      }
      else {
        return u3m_bail(c3__fail);
      }
    }
  }
  u3_noun
 u3we_shim_fun(u3_noun cor)
  {
    u3_noun van, zep, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (u3_none == (zep = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_shim_fun(zep, tub);
    }
  }

/* stag
*/
  static u3_noun
  _cqe_stag_fun(u3_noun gob,
                u3_noun sef,
                u3_noun tub)
  {
    u3_noun vex = u3x_good(u3n_slam_on(u3k(sef), u3k(tub)));
    u3_noun p_vex, q_vex;

    u3x_cell(vex, &p_vex, &q_vex);

    if ( c3n == u3du(q_vex) ) {
      return vex;
    }
    else {
      u3_noun uq_vex = u3t(q_vex);
      u3_noun puq_vex, quq_vex;
      u3_noun wag;
      u3_noun ret;

      u3x_cell(uq_vex, &puq_vex, &quq_vex);
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
 u3we_stag_fun(u3_noun cor)
  {
    u3_noun van, gob, sef, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (c3n == u3r_mean(van, u3x_sam_2, &gob, u3x_sam_3, &sef, 0)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_stag_fun(gob, sef, tub);
    }
  }

/* stew
*/
  static u3_noun
  _stew_wor(u3_noun ort,
            u3_noun wan)
  {
    if ( !_(u3a_is_cat(ort)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      if ( c3n == u3du(wan) ) {
        if ( !_(u3a_is_cat(wan)) ) {
          return u3m_bail(c3__fail);
        }
        else return (ort < wan) ? c3y : c3n;
      }
      else {
        u3_noun h_wan = u3h(wan);

        if ( !_(u3a_is_cat(h_wan)) ) {
          return u3m_bail(c3__fail);
        }
        else return (ort < h_wan) ? c3y : c3n;
      }
    }
  }

  static u3_noun
  _cqe_stew_fun(u3_noun hel,
                u3_noun tub)
  {
    u3_noun p_tub, q_tub;

    u3x_cell(tub, &p_tub, &q_tub);
    if ( c3n == u3du(q_tub) ) {
      return _fail(tub);
    }
    else {
      u3_noun iq_tub = u3h(q_tub);

      if ( !_(u3a_is_cat(iq_tub)) ) {
        return u3m_bail(c3__fail);
      }
      else while ( 1 ) {
        if ( c3n == u3du(hel) ) {
          return _fail(tub);
        }
        else {
          u3_noun n_hel, l_hel, r_hel;
          u3_noun pn_hel, qn_hel;
          c3_o    bit_o;

          u3x_trel(hel, &n_hel, &l_hel, &r_hel);
          u3x_cell(n_hel, &pn_hel, &qn_hel);

          if ( (c3n == u3du(pn_hel)) ) {
            bit_o = __((iq_tub == pn_hel));
          }
          else {
            u3_noun hpn_hel = u3h(pn_hel);
            u3_noun tpn_hel = u3t(pn_hel);

            if ( !_(u3a_is_cat(hpn_hel)) ||
                 !_(u3a_is_cat(tpn_hel)) ) {
              return _fail(tub);
            }
            else bit_o = __((iq_tub >= hpn_hel) && (iq_tub <= tpn_hel));
          }

          if ( c3y == bit_o ) {
            return u3x_good
              (u3n_slam_on(u3k(qn_hel), u3k(tub)));
          } else {
            if ( c3y == _stew_wor(iq_tub, pn_hel) ) {
              hel = l_hel;
            }
            else hel = r_hel;
          }
        }
      }
    }
  }
 u3_noun
 u3we_stew_fun(u3_noun cor)
  {
    u3_noun con, hel, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &con, 0)) ||
         (u3_none == (hel = u3r_at(2, con))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_stew_fun(hel, tub);
    }
  }

/* _stir_pair(): stack frame recording intermediate parse results
*/
  typedef struct {
    u3_noun har;  //  hair, p_vex
    u3_noun res;  //  parse-result, puq_vex
  } _stir_pair;

/* stir
*/
  static u3_noun
  _cqe_stir_fun(u3_noun rud,
                u3_noun raq,
                u3_noun fel,
                u3_noun tub)
  {
    //  pil_u: stack control structure
    //  par_u: frame pointer
    //  wag:   initial accumulator (deconstructed)
    //
    u3a_pile    pil_u;
    _stir_pair* par_u;
    u3_noun     p_wag, puq_wag, quq_wag;

    u3a_pile_prep(&pil_u, sizeof(*par_u));

    //  push incremental, successful [fel] parse results onto road stack
    //
    {
      u3_noun    vex, p_vex, q_vex, puq_vex, quq_vex;
      u3j_site fel_u;
      u3j_gate_prep(&fel_u, u3k(fel));

      vex = u3j_gate_slam(&fel_u, u3k(tub));
      u3x_cell(vex, &p_vex, &q_vex);

      u3k(tub);

      while ( u3_nul != q_vex ) {
        u3x_trel(q_vex, 0, &puq_vex, &quq_vex);

        par_u = u3a_push(&pil_u);
        par_u->har = u3k(p_vex);
        par_u->res = u3k(puq_vex);

        u3z(tub);
        tub = u3k(quq_vex);

        u3z(vex);
        vex = u3j_gate_slam(&fel_u, u3k(tub));
        u3x_cell(vex, &p_vex, &q_vex);
      }

      p_wag   = u3k(p_vex);
      puq_wag = u3k(rud);
      quq_wag = tub;

      u3z(vex);
      u3j_gate_lose(&fel_u);
    }

    //  unwind the stack, folding parse results into [wag] by way of [raq]
    //
    if ( c3n == u3a_pile_done(&pil_u) ) {
      u3j_site raq_u;
      u3j_gate_prep(&raq_u, u3k(raq));

      //  check for stack overflow
      //
      u3a_pile_sane(&pil_u);

      while ( c3n == u3a_pile_done(&pil_u) ) {
        p_wag   = _last_k(par_u->har, p_wag);
        puq_wag = u3j_gate_slam(&raq_u, u3nc(par_u->res, puq_wag));
        par_u   = u3a_pop(&pil_u);
      }

      u3j_gate_lose(&raq_u);
    }

    return u3nq(p_wag, u3_nul, puq_wag, quq_wag);
  }

  u3_noun
  u3we_stir_fun(u3_noun cor)
  {
    u3_noun van, rud, raq, fel, tub;

    if ( (c3n == u3r_mean(cor, u3x_sam, &tub, u3x_con, &van, 0)) ||
         (c3n == u3r_mean(van, u3x_sam_2, &rud,
                               u3x_sam_6, &raq,
                               u3x_sam_7, &fel,
                               0)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqe_stir_fun(rud, raq, fel, tub);
    }
  }
