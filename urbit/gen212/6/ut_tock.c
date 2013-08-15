/* j/6/tock.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/
  static u2_noun                                                  //  transfer
  _tock_in(u2_wire wir_r, 
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun peh,                                           //  retain
           u2_noun mur,                                           //  retain
           u2_noun men)                                           //  retain
  {
    if ( u2_no == u2_dust(men) ) {
      return u2_bc(wir_r, u2_nul, u2_nul);
    } 
    else {
      u2_noun i_men  = u2_h(men);
      u2_noun pi_men = u2_h(i_men);
      u2_noun qi_men = u2_t(i_men);
      u2_noun t_men  = u2_t(men);
      u2_noun geq    = j2_mcy(Pt6, ut, tack)(wir_r, van, pi_men, peh, mur);
      u2_noun p_geq  = u2_h(geq);
      u2_noun q_geq  = u2_t(geq);
      u2_noun mox    = _tock_in(wir_r, van, sut, peh, mur, t_men);
      u2_noun p_mox  = u2_h(mox);
      u2_noun q_mox  = u2_t(mox);
      u2_noun ret;

      ret = u2_bc(wir_r, 
                  ( (u2_nul == p_mox) 
                      ? u2_bc(wir_r, u2_nul, u2_rx(wir_r, p_geq)) 
                      : (u2_no == u2_sing(p_geq, u2_t(p_mox)))
                        ? u2_bl_bail(wir_r, c3__exit)
                        : u2_rx(wir_r, p_mox) ),
                  u2_bc(wir_r, u2_bc(wir_r, u2_rx(wir_r, q_geq), 
                                            u2_rx(wir_r, qi_men)),
                               u2_rx(wir_r, q_mox)));

      u2_rz(wir_r, mox);
      u2_rz(wir_r, geq);
      return ret;
    }
  }                                  

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, tock)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
  {
    u2_noun wib = _tock_in(wir_r, van, sut, peh, mur, men);
    u2_noun p_wib = u2_h(wib);
    u2_noun q_wib = u2_t(wib);

    if ( u2_nul == p_wib ) {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      u2_noun ret = u2_bc(wir_r, u2_rx(wir_r, u2_t(p_wib)), 
                                 u2_rx(wir_r, q_wib));

      u2_rz(wir_r, wib);
      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, tock)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, tock)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, peh, mur, men;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &peh, 
                                u2_cw_sam_6, &mur,
                                u2_cw_sam_7, &men,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, tock)(wir_r, van, sut, peh, mur, men);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, tock)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "tock");

    if ( u2_none == hoc ) {
      c3_assert(!"register tock");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam_2, u2_rx(wir_r, peh), 
                                           u2_cw_sam_6, u2_rx(wir_r, mur),
                                           u2_cw_sam_7, u2_rx(wir_r, men),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, tock)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, tock)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, tock)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur,                              //  retain
                        u2_noun men)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, tock)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, tock)(wir_r, van, sut, peh, mur, men);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, tock)(wir_r, van, sut, peh, mur, men);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, tock)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, tock), Tier6_c, u2_none, u2_none },
    { }
  };
