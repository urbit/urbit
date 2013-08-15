/* j/6/seek.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, seek)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hep)                              //  retain
  {
    if ( u2_nul == hep ) {
      return u2_bt
        (wir_r, _1, u2_nul, u2_rx(wir_r, sut));
    }
    else if ( u2_no == u2_dust(hep) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      u2_noun i_hep = u2_h(hep);
      u2_noun t_hep = u2_t(hep);
      u2_noun zar = j2_mcx(Pit, ut, seek)(wir_r, van, sut, way, t_hep);
      u2_noun p_zar, q_zar, r_zar;
      u2_noun pro;

      u2_as_trel(zar, &p_zar, &q_zar, &r_zar);

      if ( u2_nul != q_zar ) {
        u2_noun neg = u2_bc(wir_r, u2_bc(wir_r, u2_nul, 1), u2_nul);
        u2_noun bor = j2_mcy(Pit, ut, fire)(wir_r, van, r_zar, neg);
        u2_noun raz = u2_bt(wir_r, u2_rx(wir_r, p_zar),
                                   u2_rx(wir_r, q_zar), 
                                   bor);
   
        u2_rz(wir_r, zar);
        u2_as_trel(raz, &p_zar, &q_zar, &r_zar);
        zar = raz;
      }

      if ( u2_no == u2_dust(i_hep) ) {
        u2_noun hup = j2_mcy(Pit, ut, find)(wir_r, van, r_zar, way, i_hep);
        u2_noun p_hup, q_hup, r_hup;

        u2_as_trel(hup, &p_hup, &q_hup, &r_hup);

        pro = u2_bt
          (wir_r, j2_mbc(Pit, peg)(wir_r, p_zar, p_hup), 
                  u2_rx(wir_r, q_hup),
                  u2_rx(wir_r, r_hup));

        u2_rl_lose(wir_r, zar);
        u2_rl_lose(wir_r, hup);

        return pro;
      } 
      else {
        u2_noun hi_hep = u2_h(i_hep);
        u2_noun pi_hep = u2_t(i_hep);

        if ( (u2_nul != hi_hep) || (u2_no == u2_stud(pi_hep)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          pro = u2_bt
            (wir_r, j2_mbc(Pit, peg)(wir_r, p_zar, pi_hep),
                    u2_nul,
                    j2_mcy(Pit, ut, peek)(wir_r, van, r_zar, way, pi_hep));

          u2_rl_lose(wir_r, zar);
          return pro;
        }
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, seek)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, seek)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, hep, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &way, 
                                u2_cv_sam_3, &hep, 
                                u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pit, ut, seek)(wir_r, van, sut, way, hep);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, seek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun way,                              //  retain
                        u2_noun hep)                              //  retain
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
                                      u2_cv_sam_3, u2_rx(wir_r, hep), 
                                      0);

      if ( (u2_none == j2_mcj(Pit, ut, seek)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, seek)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, seek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun hep)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, seek)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pit, ut, seek)(wir_r, van, sut, way, hep);
      }
      else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find_trel(wir_r, fun_m, sut, way, hep);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pit, ut, seek)(wir_r, van, sut, way, hep);

          return u2_rl_save_trel(wir_r, fun_m, sut, way, hep, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, seek)(wir_r, van, sut, way, hep);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      c3_assert(pro != u2_none);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pit, ut, seek)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, way, hep, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &way, 
                                u2_cv_sam_3, &hep,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rt
        (wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, way), u2_rx(wir_r, hep));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, seek)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pit, ut, seek), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pit, ut, seek)
    },
    { }
  };

