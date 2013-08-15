/* j/6/firm.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, firm)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_noun p_sut, q_sut, r_sut;

    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: 
      {
        return u2_yes;
      }
      case c3__void:
      {
        return u2_no;
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: p_sut = u2_t(sut);
      {
        return u2_stud(dib);
      }
      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_and
          (u2_dust(dib),
           u2_and(j2_mcx(Pt6, ut, firm)(wir_r, van, p_sut, u2_h(dib)),
                  j2_mcx(Pt6, ut, firm)(wir_r, van, q_sut, u2_t(dib))));
      }
      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

        u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
        u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);

        return u2_and
          (u2_dust(dib),
           u2_and
            (j2_mcx(Pt6, ut, firm)(wir_r, van, p_sut, u2_h(dib)),
             ((u2_nul == prq_sut) ? u2_bl_error(wir_r, "firm-core")
                                  : u2_sing(prq_sut, u2_t(dib)))));
      }
      case c3__cube: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_sing(dib, p_sut);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return j2_mcx(Pt6, ut, firm)(wir_r, van, q_sut, dib);
      }
      case c3__fine: u2_bi_trel(wir_r, u2_t(sut), &p_sut, &q_sut, &r_sut);
      {
        if ( u2_no == j2_mcx(Pt6, ut, firm)(wir_r, van, r_sut, dib) ) {
          return u2_no;
        } else {
          u2_noun feg = j2_mcy(Pt6, ut, bust)(wir_r, van, q_sut, dib);
          u2_flag ret;

          switch ( p_sut ) {
            default: return u2_bl_bail(wir_r, c3__fail); 
            case c3__pure: ret = u2_sing(dib, feg);
            case c3__very: ret = u2_sing(u2_yes, feg);
          }
          u2_rz(wir_r, feg);
          return ret;
        }
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_or(j2_mcx(Pt6, ut, firm)(wir_r, van, p_sut, dib),
                     j2_mcx(Pt6, ut, firm)(wir_r, van, q_sut, dib));
      }
      case c3__hold: 
      {
        u2_noun goy = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun ret = j2_mcx(Pt6, ut, firm)(wir_r, van, goy, dib);

        u2_rz(wir_r, goy);
        return ret;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, firm)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, firm)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &dib, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, firm)(wir_r, van, sut, dib);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, firm)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dib)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "firm");

    if ( u2_none == hoc ) {
      c3_assert(!"register firm");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, dib), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, firm)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, firm)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, firm)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, firm)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, firm)(wir_r, van, sut, dib);
      }
      else {
        c3_m    fun_m = c3__firm;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, dib);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, firm)(wir_r, van, sut, dib);

          return u2_rl_save_cell(wir_r, fun_m, sut, dib, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, firm)(wir_r, van, sut, dib);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, firm)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &dib, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, dib));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, firm)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, firm), 
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, firm), c3__firm
    },
    { }
  };
