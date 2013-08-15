/* j/6/snap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, snap)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_noun p_sut, q_sut;
    u2_noun ret;

    if ( u2_no == u2_dust(sut) ) {
      return u2_rx(wir_r, gen);
    }
    else switch ( u2_h(sut) ) {
      default: return u2_rx(wir_r, gen);

      case c3__cell: {
        if ( (u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          u2_noun ter = u2_frag(u2_cw_con_2, van);
          u2_noun haq = j2_mcy(Pt6, ap, hack)(wir_r, ter, gen);

          if ( u2_yes == u2_h(haq) ) {
            u2_noun p_haq, q_haq;

            u2_as_cell(u2_t(haq), &p_haq, &q_haq);

            ret = u2_bc
              (wir_r, j2_mcx(Pt6, ut, snap)(wir_r, van, p_sut, p_haq),
                      j2_mcx(Pt6, ut, snap)(wir_r, van, q_sut, q_haq));
          } else {
            ret = u2_rx(wir_r, u2_t(haq));
          }
          u2_rl_lose(wir_r, haq);
          return ret;
        }
      }
      case c3__face: {
        if ( (u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          return u2_bt
            (wir_r, c3__ktts,
                    u2_rx(wir_r, p_sut),
                    j2_mcx(Pt6, ut, snap)(wir_r, van, q_sut, gen));
        }
      }
      case c3__fork: {
          u2_noun ter = u2_frag(u2_cw_con_2, van);
          u2_noun haq = j2_mcy(Pt6, ap, hack)(wir_r, ter, gen);

          if ( u2_yes == u2_h(haq) ) {
            u2_noun p_haq, q_haq;
            u2_noun hed = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__read, 2);
            u2_noun tal = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__read, 3);

            u2_as_cell(u2_t(haq), &p_haq, &q_haq);

            ret = u2_bc
              (wir_r, j2_mcx(Pt6, ut, snap)(wir_r, van, hed, p_haq),
                      j2_mcx(Pt6, ut, snap)(wir_r, van, tal, q_haq));

            u2_rz(wir_r, hed);
            u2_rz(wir_r, tal);
          } else {
            ret = u2_rx(wir_r, u2_t(haq));
          }
          u2_rl_lose(wir_r, haq);
          return ret;
        }

      case c3__fine: 
      case c3__cube:
      case c3__hold: {
        p_sut = u2_t(sut);
        {
          u2_noun fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
          
          ret = j2_mcx(Pt6, ut, snap)(wir_r, van, fop, gen);
          u2_rl_lose(wir_r, fop);

          return ret;
        }
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, snap)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, snap)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, gen;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &gen, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, snap)(wir_r, van, sut, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, snap)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "snap");

    if ( u2_none == hoc ) {
      c3_assert(!"register snap");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, gen), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, snap)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, snap)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, snap)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, snap)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, snap)(wir_r, van, sut, gen);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, snap)(wir_r, van, sut, gen);
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
  j2_mcj(Pt6, ut, snap)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, snap), Tier6_b, u2_none, u2_none },
    { }
  };
