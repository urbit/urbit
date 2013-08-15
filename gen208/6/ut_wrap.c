/* j/6/wrap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun                                                  //  produce
  j2_mcx(Pt6, ut, wrap)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2_dust(sut) )  {
      return u2_bl_error(wir_r, "wrap-type");
    }  
    else switch ( u2_h(sut) ) {
      default: return u2_bl_error(wir_r, "wrap-type");

      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun pq_sut, qq_sut, rq_sut;

        if ( u2_no == u2_as_trel(q_sut, &pq_sut, &qq_sut, &rq_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else if ( c3__gold != pq_sut ) {
          return u2_bl_error(wir_r, "wrap-gold");
        }
        else {
          return u2_bt(wir_r, c3__core,
                              u2_rx(wir_r, p_sut),
                              u2_bt(wir_r, u2_rx(wir_r, yoz),
                                           u2_rx(wir_r, qq_sut),
                                           u2_rx(wir_r, rq_sut)));
        }
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return j2_mby(Pt6, fork)
          (wir_r, j2_mcx(Pt6, ut, wrap)(wir_r, van, p_sut, yoz),
                  j2_mcx(Pt6, ut, wrap)(wir_r, van, q_sut, yoz));
      }
      case c3__hold: 
      {
        u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun pro = j2_mcx(Pt6, ut, wrap)(wir_r, van, fop, yoz);

        u2_rl_lose(wir_r, fop);
        return pro;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, wrap)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, wrap)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, yoz, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &yoz, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, wrap)(wir_r, van, sut, yoz);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, wrap)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun yoz)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "wrap");

    if ( u2_none == hoc ) {
      c3_assert(!"register wrap");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, yoz), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, wrap)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, wrap)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, wrap)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, wrap)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, wrap)(wir_r, van, sut, yoz);
      }
      else {
        c3_m    fun_m = c3__wrap;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, yoz);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, wrap)(wir_r, van, sut, yoz);

          return u2_rl_save_cell(wir_r, fun_m, sut, yoz, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, wrap)(wir_r, van, sut, yoz);
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
  j2_mcj(Pt6, ut, wrap)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, wrap), Tier6_b, u2_none, u2_none },
    { }
  };
