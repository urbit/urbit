/* j/6/seek.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun                                                  //  produce
  _seek_flat(u2_wire wir_r,
             u2_noun wob)                                         //  retain
  {
    if ( u2_nul == wob ) {
      return u2_nul;
    } else {
      u2_noun i_wob = u2_h(wob);
      u2_noun t_wob = u2_t(wob);

      return u2_bc
        (wir_r, u2_bc(wir_r, u2_rx(wir_r, u2_h(i_wob)),
                             u2_bt(wir_r, c3__ash, u2_nul, _1)),
                _seek_flat(wir_r, t_wob));
    }
  }

  static u2_noun                                                  //  transfer
  _seek_silk_yew(u2_wire wir_r, 
                 u2_noun van,                                     //  retain
                 u2_noun syx,                                     //  retain
                 u2_noun qq_tor)                                  //  retain
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    else {
      u2_noun iqq_tor  = u2_h(qq_tor);
      u2_noun qiqq_tor = u2_t(iqq_tor);
      u2_noun yon      = _seek_silk_yew(wir_r, van, syx, u2_t(qq_tor));

      if ( c3__yew != u2_h(qiqq_tor) ) {
        return yon;
      } else {
        u2_noun nuy = j2_mby(Pt6, look)(wir_r, syx, u2_t(qiqq_tor));

        if ( u2_nul == nuy ) {
          return u2_bl_error(wir_r, "silk");
        }
        else {
          yon = u2_bc(wir_r, u2_rx(wir_r, u2_t(nuy)), yon);
          u2_rz(wir_r, nuy);
          return yon;
        }
      }
    } 
  }

#if 0
  static u2_noun                                                  //  retain
  _seek_silk_yaw(u2_noun wir_r,
                 u2_noun hey)                                     //  retain
  {
    u2_atom axe = 0;

    while ( u2_nul != hey ) {
      if ( axe == 0 ) {
        axe = u2_h(u2_h(hey));
      } else if ( axe != u2_h(u2_h(hey)) ) {
        return u2_bl_error(wir_r, "silk");
      }
      hey = u2_t(hey);
    }
  }

  static u2_noun                                                  //  transfer
  _seek_silk_fum(u2_noun wir_r,
                 u2_noun hey,                                     //  retain
                 u2_noun qq_tor)
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    c3_assert(u2_nul != hey);
    return u2_bc
      (wir_r, u2_bc(wir_r, u2_rx(wir_r, u2_h(u2_h(qq_tor))),
                           u2_rx(wir_r, u2_t(u2_h(hey)))),
              _seek_silk_fum(wir_r, u2_t(hey), u2_t(qq_tor)));
  }

  static u2_noun                                                  //  transfer
  _seek_silk(u2_wire wir_r, 
             u2_noun van,                                         //  retain
             u2_noun syx,                                         //  retain
             u2_noun tor)                                         //  retain
  {
    u2_noun p_tor, q_tor, pq_tor, qq_tor;
    u2_noun hey, ret;

    u2_as_cell(tor, &p_tor, &q_tor);
    if ( u2_yes == u2_h(q_tor) ) {
      return u2_nul;
    }
    u2_as_cell(u2_t(q_tor), &pq_tor, &qq_tor);
   
    hey = _seek_silk_yew(wir_r, van, syx, qq_tor);
    if ( u2_nul == hey ) {
      return u2_nul;
    }
    if ( u2_ckb_lent(u2_rx(wir_r, hey)) != 
         u2_ckb_lent(u2_rx(wir_r, qq_tor)) ) 
    {
      return u2_bl_error(wir_r, "silk");
    }

    ret = u2_bq
      (wir_r, u2_nul, 
              u2_no,
              j2_mbc(Pt3, peg)(wir_r, pq_tor, _seek_silk_yaw(wir_r, hey)),
              _seek_silk_fum(wir_r, hey, qq_tor));

    u2_rz(wir_r, hey);
    return ret;
  }
#endif

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, seek)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    if ( u2_nul == hyp ) {
      return u2_bt
        (wir_r, _1, u2_yes, u2_rx(wir_r, sut));
    }
    else if ( u2_no == u2_dust(hyp) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      u2_noun i_hyp = u2_h(hyp);
      u2_noun t_hyp = u2_t(hyp);
      u2_noun zar; 
      u2_noun p_zar, q_zar;
      u2_noun yip, sic, syp, ret;

      if ( u2_yes == u2_dust(i_hyp) ) {
        yip = u2_rx(wir_r, i_hyp);
      } else {
        yip = u2_bt(wir_r, u2_no, 0, u2_rx(wir_r, i_hyp));
      }

      zar = j2_mcx(Pt6, ut, seek)(wir_r, van, sut, way, t_hyp);
      u2_as_cell(zar, &p_zar, &q_zar);

      if ( u2_yes == u2_h(yip) ) {
        sic = u2_nul;
      } else {
        // sic = _seek_silk(wir_r, van, u2_h(u2_t(yip)), zar);
        sic = u2_nul;
      }
      if ( u2_nul != sic ) {
        u2_rz(wir_r, yip);
        u2_rz(wir_r, zar);

        return u2_t(sic);
      }

      if ( u2_yes == u2_h(q_zar) ) {
        syp = u2_rx(wir_r, u2_t(q_zar));
      } else {
        u2_noun pq_zar, qq_zar;
        u2_noun wip;

        u2_as_cell(u2_t(q_zar), &pq_zar, &qq_zar);
        wip = _seek_flat(wir_r, qq_zar);
        syp = j2_mcy(Pt6, ut, fire)(wir_r, van, sut, wip);

        u2_rz(wir_r, wip);
      }

      if ( u2_no == u2_h(yip) ) {
        u2_noun p_yip, q_yip, hud;

        if ( u2_no == u2_as_cell(u2_t(yip), &p_yip, &q_yip) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        hud = j2_mcy(Pt6, ut, fink)(wir_r, van, syp, p_yip, way, q_yip);
        {
          u2_noun p_hud, q_hud;

          u2_as_cell(hud, &p_hud, &q_hud);

          ret = u2_bc(wir_r, j2_mbc(Pt3, peg)(wir_r, p_zar, p_hud), 
                             u2_rx(wir_r, q_hud));
          u2_rz(wir_r, hud);
        }
      } 
      else {
        u2_noun p_yip = u2_t(yip);

        if ( u2_no == u2_stud(p_yip) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          ret = u2_bt
            (wir_r, j2_mbc(Pt3, peg)(wir_r, p_zar, p_yip),
                    u2_yes,
                    j2_mcy(Pt6, ut, peek)(wir_r, van, syp, way, p_yip));
        }
      }
      u2_rz(wir_r, yip);
      u2_rz(wir_r, syp);
      u2_rz(wir_r, zar);
      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, seek)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, seek)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, hyp, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &way, 
                                u2_cv_sam_3, &hyp, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, seek)(wir_r, van, sut, way, hyp);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, seek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "seek");

    if ( u2_none == hoc ) {
      c3_assert(!"register seek");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cv_sam_2, u2_rx(wir_r, way), 
                                      u2_cv_sam_3, u2_rx(wir_r, hyp), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, seek)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, seek)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, seek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hyp)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, seek)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, seek)(wir_r, van, sut, way, hyp);
      }
      else {
        c3_m    fun_m = c3__seek;
        u2_noun pro   = u2_rl_find_trel(wir_r, fun_m, sut, way, hyp);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, seek)(wir_r, van, sut, way, hyp);

          return u2_rl_save_trel(wir_r, fun_m, sut, way, hyp, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, seek)(wir_r, van, sut, way, hyp);
      fol = u2_h(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, seek)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, way, hyp, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &way, 
                                u2_cv_sam_3, &hyp, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rt
        (wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, way), u2_rx(wir_r, hyp));
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

