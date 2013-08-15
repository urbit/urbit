/* j/5/parse.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun                                                  //  produce
  _slip(u2_wire wir_r,
        u2_noun weq,                                              //  retain
        u2_noun naz)                                              //  retain
  {
    u2_noun p_naz, q_naz;

    u2_bi_cell(wir_r, naz, &p_naz, &q_naz);
    if ( 10 == weq ) {
      return u2_bc(wir_r, u2_bl_good(wir_r, 
                                     u2_rl_vint(wir_r, u2_rx(wir_r, p_naz))),
                          1);
    } else {
      return u2_bc(wir_r, u2_rx(wir_r, p_naz),
                          u2_bl_good(wir_r,
                                     u2_rl_vint(wir_r, u2_rx(wir_r, q_naz))));
    }
  }

  static u2_noun                                                  //  produce
  _fail(u2_wire wir_r,
        u2_noun tub)                                              //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);
    return u2_bc(wir_r, u2_rx(wir_r, p_tub), u2_nul);
  }

  static u2_noun                                                  //  produce
  _last(u2_wire wir_r,
        u2_noun zyc,                                              //  retain
        u2_noun naz)                                              //  retain
  {
    u2_noun p_zyc, q_zyc, p_naz, q_naz;

    u2_bi_cell(wir_r, zyc, &p_zyc, &q_zyc);
    u2_bi_cell(wir_r, naz, &p_naz, &q_naz);

    if ( !u2_fly_is_cat(p_zyc) || !u2_fly_is_cat(q_zyc) ||
         !u2_fly_is_cat(p_naz) || !u2_fly_is_cat(q_naz) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      if ( p_zyc == p_naz ) {
        return (q_zyc > q_naz) ? u2_rx(wir_r, zyc) : u2_rx(wir_r, naz);
      }
      else {
        return (p_zyc > p_naz) ? u2_rx(wir_r, zyc) : u2_rx(wir_r, naz);
      }
    }
  }

  static u2_noun                                                  //  produce
  _next(u2_wire wir_r,
        u2_noun tub)                                              //  retain
  {
    u2_noun p_tub, q_tub;
    u2_noun zac;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);
    if ( u2_no == u2_dust(q_tub) ) {
      return _fail(wir_r, tub);
    } 
    else {
      u2_noun iq_tub = u2_h(q_tub);
      u2_noun tq_tub = u2_t(q_tub);

      zac = _slip(wir_r, iq_tub, p_tub);

      return u2_bc
        (wir_r, zac,
                u2_bq(wir_r, u2_nul,
                             u2_rx(wir_r, iq_tub),
                             u2_rx(wir_r, zac),
                             u2_rx(wir_r, tq_tub)));
    }
  }

/* bend
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt5, bend, fun)(u2_wire wir_r,
                         u2_noun raq,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
  {
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_vex)));
      
      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      yur = _last(wir_r, p_vex, p_yit);

      if ( u2_no == u2_dust(q_yit) ) {
        ret = u2_bc(wir_r, yur, u2_rx(wir_r, q_vex));
      } 
      else {
        u2_noun uq_yit = u2_t(q_yit);
        u2_noun puq_yit, quq_yit;
         
        u2_bi_cell(wir_r, uq_yit, &puq_yit, &quq_yit);
        ret = u2_bq(wir_r, yur,
                           u2_nul,
                           u2_bl_good
                            (wir_r, 
                             u2_nk_mong
                               (wir_r, raq, 
                                       u2_bc(wir_r, u2_rx(wir_r, puq_vex),
                                                    u2_rx(wir_r, puq_yit)))),
                           u2_rx(wir_r, quq_yit));
      }
      u2_rz(wir_r, yit);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, bend, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, bend, fun)(wir_r, raq, vex, sab);
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
  j2_mcx(Pt5, cold, fun)(u2_wire wir_r,
                         u2_noun cus,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun vex = u2_bl_good(wir_r, u2_nk_mong(wir_r, sef, u2_rx(wir_r, tub)));
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);

    if ( u2_no == u2_dust(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun quq_vex;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, 0, &quq_vex);
      ret = u2_bq(wir_r, u2_rx(wir_r, p_vex),
                         u2_nul,
                         u2_rx(wir_r, cus),
                         u2_rx(wir_r, quq_vex));

      u2_rz(wir_r, vex);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, cold, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, cus, sef, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_mean(van, u2_cv_sam_2, &cus, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, cold, fun)(wir_r, cus, sef, tub);
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
  j2_mcx(Pt5, cook, fun)(u2_wire wir_r,
                         u2_noun poq,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun vex = u2_bl_good(wir_r, u2_nk_mong(wir_r, sef, u2_rx(wir_r, tub)));
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);

    if ( u2_no == u2_dust(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      wag = u2_bl_good(wir_r, u2_nk_mong(wir_r, poq, u2_rx(wir_r, puq_vex)));
      ret = u2_bq(wir_r, u2_rx(wir_r, p_vex),
                         u2_nul,
                         wag,
                         u2_rx(wir_r, quq_vex));

      u2_rz(wir_r, vex);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, cook, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, poq, sef, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_mean(van, u2_cv_sam_2, &poq, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, cook, fun)(wir_r, poq, sef, tub);
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
  j2_mcx(Pt5, comp, fun)(u2_wire wir_r,
                         u2_noun raq,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
  {
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_vex)));
      
      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      yur = _last(wir_r, p_vex, p_yit);

      if ( u2_no == u2_dust(q_yit) ) {
        ret = u2_bc(wir_r, yur, u2_rx(wir_r, q_yit));
      } 
      else {
        u2_noun uq_yit = u2_t(q_yit);
        u2_noun puq_yit, quq_yit;
         
        u2_bi_cell(wir_r, uq_yit, &puq_yit, &quq_yit);
        ret = u2_bq(wir_r, yur,
                           u2_nul,
                           u2_bl_good
                            (wir_r, 
                             u2_nk_mong
                               (wir_r, raq, 
                                       u2_bc(wir_r, u2_rx(wir_r, puq_vex),
                                                    u2_rx(wir_r, puq_yit)))),
                           u2_rx(wir_r, quq_yit));
      }
      u2_rz(wir_r, yit);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, comp, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, raq, vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (raq = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, comp, fun)(wir_r, raq, vex, sab);
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
  j2_mcx(Pt5, easy, fun)(u2_wire wir_r,
                         u2_noun huf,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);
    return u2_bq(wir_r, u2_rx(wir_r, p_tub),
                        u2_nul,
                        u2_rx(wir_r, huf),
                        u2_rx(wir_r, tub));
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, easy, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, huf, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (huf = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, easy, fun)(wir_r, huf, tub);
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
  j2_mcx(Pt5, glue, fun)(u2_wire wir_r,
                         u2_noun bus,                             //  retain
                         u2_noun vex,                             //  retain
                         u2_noun sab)                             //  retain
  {
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } 
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, bus, u2_rx(wir_r, quq_vex)));
      
      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      yur = _last(wir_r, p_vex, p_yit);

      if ( u2_no == u2_dust(q_yit) ) {
        ret = u2_bc(wir_r, yur, u2_nul);
      } 
      else {
        u2_noun uq_yit = u2_t(q_yit);
        u2_noun puq_yit, quq_yit;
        u2_noun wam, p_wam, q_wam, goy;
         
        u2_bi_cell(wir_r, uq_yit, &puq_yit, &quq_yit);
        wam = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_yit)));

        u2_bi_cell(wir_r, wam, &p_wam, &q_wam);
        goy = _last(wir_r, yur, p_wam);
        u2_rz(wir_r, yur);

        if ( u2_no == u2_dust(q_wam) ) {
          ret = u2_bc(wir_r, goy, u2_nul);
        } else {
          u2_noun uq_wam = u2_t(q_wam);
          u2_noun puq_wam, quq_wam;

          u2_bi_cell(wir_r, uq_wam, &puq_wam, &quq_wam);
          ret = u2_bq(wir_r, goy,
                             u2_nul,
                             u2_bc(wir_r, u2_rx(wir_r, puq_vex),
                                          u2_rx(wir_r, puq_wam)),
                             u2_rx(wir_r, quq_wam));
        }
        u2_rz(wir_r, wam);
      }
      u2_rz(wir_r, yit);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, glue, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, bus, vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex,
                                u2_cv_sam_3, &sab,
                                u2_cv_con, &van, 0)) ||
         (u2_none == (bus = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, glue, fun)(wir_r, bus, vex, sab);
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
  j2_mcx(Pt5, here, fun)(u2_wire wir_r,
                         u2_noun hez,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun vex = u2_bl_good(wir_r, u2_nk_mong(wir_r, sef, u2_rx(wir_r, tub)));
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);

    if ( u2_no == u2_dust(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun p_tub, q_tub;
      u2_noun puq_vex, quq_vex, pquq_vex;
      u2_noun gud, wag;
      u2_noun ret;

      u2_bi_cell(wir_r, tub, &p_tub, &q_tub);
      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      u2_bi_cell(wir_r, quq_vex, &pquq_vex, 0);
      gud = u2_bc(wir_r, 
                  u2_bc(wir_r, u2_rx(wir_r, p_tub), 
                               u2_rx(wir_r, pquq_vex)),
                  u2_rx(wir_r, puq_vex));

      wag = u2_bl_good(wir_r, u2_nk_mong(wir_r, hez, gud));
      ret = u2_bq(wir_r, u2_rx(wir_r, p_vex),
                         u2_nul,
                         wag,
                         u2_rx(wir_r, quq_vex));

      u2_rz(wir_r, vex);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, here, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, hez, sef, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_mean(van, u2_cv_sam_2, &hez, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, here, fun)(wir_r, hez, sef, tub);
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
  j2_mcx(Pt5, just, fun)(u2_wire wir_r,
                         u2_noun daf,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);

    if ( u2_no == u2_dust(q_tub) ) {
      return _fail(wir_r, tub);
    }
    else {
      u2_noun iq_tub = u2_h(q_tub);

      if ( u2_yes == u2_sing(daf, iq_tub) ) {
        return _next(wir_r, tub);
      }
      else return _fail(wir_r, tub);
    }
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, just, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, daf, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (daf = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, just, fun)(wir_r, daf, tub);
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
  j2_mcx(Pt5, mask, fun)(u2_wire wir_r,
                         u2_noun bud,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);

    if ( u2_no == u2_dust(q_tub) ) {
      return _fail(wir_r, tub);
    }
    else {
      u2_noun iq_tub = u2_h(q_tub);

      while ( u2_yes == u2_dust(bud) ) {
        if ( u2_yes == u2_sing(u2_h(bud), iq_tub) ) {
          return _next(wir_r, tub);
        }
        bud = u2_t(bud);
      }
      return _fail(wir_r, tub);
    }
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, mask, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, bud, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (bud = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, mask, fun)(wir_r, bud, tub);
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
  j2_mbx(Pt5, pfix)(u2_wire wir_r, 
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
  {
    u2_noun p_vex, q_vex;
    
    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } 
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, p_yit, q_yit;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_vex)));

      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      ret = u2_bc
        (wir_r,
         _last(wir_r, p_vex, p_yit),
         u2_rx(wir_r, q_yit));

      u2_rz(wir_r, yit);
      return ret;
    }
  }
  u2_noun                                                         //  produce
  j2_mb(Pt5, pfix)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbx(Pt5, pfix)(wir_r, vex, sab);
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
  j2_mbx(Pt5, plug)(u2_wire wir_r,
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
  {
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } 
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, yur;
      u2_noun p_yit, q_yit;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_vex)));
      
      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      yur = _last(wir_r, p_vex, p_yit);

      if ( u2_no == u2_dust(q_yit) ) {
        ret = u2_bc(wir_r, yur, u2_rx(wir_r, q_yit));
      } 
      else {
        u2_noun uq_yit = u2_t(q_yit);
        u2_noun puq_yit, quq_yit;
         
        u2_bi_cell(wir_r, uq_yit, &puq_yit, &quq_yit);
        ret = u2_bq(wir_r, yur,
                           u2_nul,
                           u2_bc(wir_r, u2_rx(wir_r, puq_vex),
                                        u2_rx(wir_r, puq_yit)),
                           u2_rx(wir_r, quq_yit));
      }
      u2_rz(wir_r, yit);
      return ret;
    }
  }
  u2_noun                                                         //  produce
  j2_mb(Pt5, plug)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbx(Pt5, plug)(wir_r, vex, sab);
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
  j2_mbx(Pt5, pose)(u2_wire wir_r, 
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
  {
    u2_noun p_vex, q_vex;
    
    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_yes == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } else {
      u2_noun roq = u2_bl_good(wir_r, u2_nk_kick(wir_r, sab));
      u2_noun p_roq, q_roq;
      u2_noun ret;

      u2_bi_cell(wir_r, roq, &p_roq, &q_roq);
      ret = u2_bc
        (wir_r,
         _last(wir_r, p_vex, p_roq),
         u2_rx(wir_r, q_roq));

      u2_rz(wir_r, roq);
      return ret;
    }
  }
  u2_noun                                                         //  produce
  j2_mb(Pt5, pose)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbx(Pt5, pose)(wir_r, vex, sab);
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
  j2_mbx(Pt5, sfix)(u2_wire wir_r, 
                    u2_noun vex,                                  //  retain
                    u2_noun sab)                                  //  retain
  {
    u2_noun p_vex, q_vex;
    
    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      return u2_rx(wir_r, vex);
    } 
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun yit, p_yit, q_yit, yur;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      yit = u2_bl_good(wir_r, u2_nk_mong(wir_r, sab, u2_rx(wir_r, quq_vex)));

      u2_bi_cell(wir_r, yit, &p_yit, &q_yit);
      yur = _last(wir_r, p_vex, p_yit);

      if ( u2_no == u2_dust(q_yit) ) {
        ret = u2_bc(wir_r, yur, u2_nul);
      } 
      else {
        u2_noun uq_yit = u2_t(q_yit);
        u2_noun puq_yit, quq_yit;
        
        u2_bi_cell(wir_r, uq_yit, &puq_yit, &quq_yit);

        ret = u2_bq(wir_r, yur, 
                           u2_nul,
                           u2_rx(wir_r, puq_vex),
                           u2_rx(wir_r, quq_yit));
      }
      u2_rz(wir_r, yit);
      return ret;
    }
  }
  u2_noun                                                         //  produce
  j2_mb(Pt5, sfix)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun vex, sab;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &vex, u2_cv_sam_3, &sab, 0)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbx(Pt5, sfix)(wir_r, vex, sab);
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
  j2_mcx(Pt5, shim, fun)(u2_wire wir_r,
                         u2_noun zep,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);

    if ( u2_no == u2_dust(q_tub) ) {
      return _fail(wir_r, tub);
    }
    else {
      u2_noun p_zep, q_zep;
      u2_noun iq_tub = u2_h(q_tub);

      u2_bi_cell(wir_r, zep, &p_zep, &q_zep);
      if ( u2_fly_is_cat(p_zep) && 
           u2_fly_is_cat(q_zep) &&
           u2_fly_is_cat(iq_tub) )
      {
        if ( (iq_tub >= p_zep) && (iq_tub <= q_zep) ) {
          return _next(wir_r, tub);
        }
        else return _fail(wir_r, tub);
      }
      else {
        return u2_bl_bail(wir_r, c3__fail);
      }
    }
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, shim, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, zep, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_none == (zep = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, shim, fun)(wir_r, zep, tub);
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
  j2_mcx(Pt5, stag, fun)(u2_wire wir_r,
                         u2_noun gob,                             //  retain
                         u2_noun sef,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun vex = u2_bl_good(wir_r, u2_nk_mong(wir_r, sef, u2_rx(wir_r, tub)));
    u2_noun p_vex, q_vex;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);

    if ( u2_no == u2_dust(q_vex) ) {
      return vex;
    }
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag;
      u2_noun ret;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      wag = u2_bc(wir_r, u2_rx(wir_r, gob), u2_rx(wir_r, puq_vex));
      ret = u2_bq(wir_r, u2_rx(wir_r, p_vex),
                         u2_nul,
                         wag,
                         u2_rx(wir_r, quq_vex));

      u2_rz(wir_r, vex);
      return ret;
    }
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, stag, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, gob, sef, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_mean(van, u2_cv_sam_2, &gob, u2_cv_sam_3, &sef, 0)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, stag, fun)(wir_r, gob, sef, tub);
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
  _stew_wor(u2_wire wir_r,
            u2_noun ort,
            u2_noun wan)
  {
    if ( !u2_fly_is_cat(ort) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      if ( u2_no == u2_dust(wan) ) {
        if ( !u2_fly_is_cat(wan) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else return (ort < wan) ? u2_yes : u2_no;
      }
      else {
        u2_noun h_wan = u2_h(wan);

        if ( !u2_fly_is_cat(h_wan) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else return (ort < h_wan) ? u2_yes : u2_no;
      }
    }
  }

  u2_noun                                                         //  produce
  j2_mcx(Pt5, stew, fun)(u2_wire wir_r,
                         u2_noun hel,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun p_tub, q_tub;

    u2_bi_cell(wir_r, tub, &p_tub, &q_tub);
    if ( u2_no == u2_dust(q_tub) ) {
      return _fail(wir_r, tub);
    }
    else {
      u2_noun iq_tub = u2_h(q_tub);

      if ( !u2_fly_is_cat(iq_tub) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else while ( 1 ) {
        if ( u2_no == u2_dust(hel) ) {
          return _fail(wir_r, tub);
        } 
        else {
          u2_noun n_hel, l_hel, r_hel;
          u2_noun pn_hel, qn_hel;
          c3_t    bit_t;

          u2_bi_trel(wir_r, hel, &n_hel, &l_hel, &r_hel);
          u2_bi_cell(wir_r, n_hel, &pn_hel, &qn_hel);

          if ( (u2_no == u2_dust(pn_hel)) ) {
            bit_t = (iq_tub == pn_hel);
          }
          else {
            u2_noun hpn_hel = u2_h(pn_hel);
            u2_noun tpn_hel = u2_t(pn_hel);

            if ( !u2_fly_is_cat(hpn_hel) || !u2_fly_is_cat(tpn_hel) ) {
              return _fail(wir_r, tub);
            }
            else bit_t = (iq_tub >= hpn_hel) && (iq_tub <= tpn_hel);
          }

          if ( bit_t ) {
            return u2_bl_good
              (wir_r, u2_nk_mong(wir_r, qn_hel, u2_rx(wir_r, tub)));
          } else {
            if ( u2_yes == _stew_wor(wir_r, iq_tub, pn_hel) ) {
              hel = l_hel;
            }
            else hel = r_hel;
          }
        }
      }
    }
  }
  u2_noun                                                         //  produce
  j2_mc(Pt5, stew, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun con, hel, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &con, 0)) ||
         (u2_none == (hel = u2_frag(2, con))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, stew, fun)(wir_r, hel, tub);
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
  j2_mcx(Pt5, stir, fun)(u2_wire wir_r,
                         u2_noun rud,                             //  retain
                         u2_noun raq,                             //  retain
                         u2_noun fel,                             //  retain
                         u2_noun tub)                             //  retain
  {
    u2_noun vex = u2_bl_good(wir_r, u2_nk_mong(wir_r, fel, u2_rx(wir_r, tub)));
    u2_noun p_vex, q_vex;
    u2_noun ret;

    u2_bi_cell(wir_r, vex, &p_vex, &q_vex);
    if ( u2_no == u2_dust(q_vex) ) {
      ret = u2_bq(wir_r, u2_rx(wir_r, p_vex),
                         u2_nul,
                         u2_rx(wir_r, rud),
                         u2_rx(wir_r, tub));
    }
    else {
      u2_noun uq_vex = u2_t(q_vex);
      u2_noun puq_vex, quq_vex;
      u2_noun wag, p_wag, q_wag, uq_wag, puq_wag, quq_wag;

      u2_bi_cell(wir_r, uq_vex, &puq_vex, &quq_vex);
      wag = j2_mcx(Pt5, stir, fun)(wir_r, rud, raq, fel, quq_vex);

      u2_bi_cell(wir_r, wag, &p_wag, &q_wag);
      if ( u2_no == u2_dust(q_wag) ) {
        return u2_bl_bail(wir_r, c3__fail);
      }
      uq_wag = u2_t(q_wag);
      u2_bi_cell(wir_r, uq_wag, &puq_wag, &quq_wag);

      ret = u2_bq
        (wir_r, _last(wir_r, p_vex, p_wag),
                u2_nul,
                u2_bl_good(wir_r, 
                          u2_nk_mong
                            (wir_r, raq, 
                                    u2_bc(wir_r, u2_rx(wir_r, puq_vex),
                                                 u2_rx(wir_r, puq_wag)))),
                u2_rx(wir_r, quq_wag));
      u2_rz(wir_r, wag);
    }
    u2_rz(wir_r, vex);
    return ret;
  }

  u2_noun                                                         //  produce
  j2_mc(Pt5, stir, fun)(u2_wire wir_r, 
                        u2_noun cor)                              //  retain
  {
    u2_noun van, rud, raq, fel, tub;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &tub, u2_cv_con, &van, 0)) ||
         (u2_no == u2_mean(van, u2_cv_sam_2, &rud, 
                                u2_cv_sam_6, &raq, 
                                u2_cv_sam_7, &fel, 
                                0)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt5, stir, fun)(wir_r, rud, raq, fel, tub);
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
