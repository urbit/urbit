/* j/6/ut_burn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _burn_in(u2_wire wir_r, 
           u2_noun van,
           u2_noun sut,
           u2_noun gil)
  {
    u2_noun p_sut, q_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: return _0;
      case c3__void: {
        return u2_bl_error(wir_r, "burn-void");
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: return _0;
      case c3__bull: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_bl_error(wir_r, "burn-bull");
      }
      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_bc
          (wir_r, _burn_in(wir_r, van, p_sut, gil),
                  _burn_in(wir_r, van, q_sut, gil));
      }
      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
                     u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
                     u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);
      {
        return u2_bc
          (wir_r, u2_rx(wir_r, prq_sut),
                  _burn_in(wir_r, van, p_sut, gil));
      }
      case c3__cube: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_rx(wir_r, p_sut);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return _burn_in(wir_r, van, q_sut, gil);
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return _burn_in(wir_r, van, p_sut, gil);
      }
      case c3__hold: p_sut = u2_t(sut);
      {
        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, sut)) ) {
          u2_noun sux = j2_mcy(Pt6, ut, dunq)(wir_r, van, "type", sut);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, sux));
          return u2_bl_error(wir_r, "burn-loop");
        } 
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, sut);
          u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
          u2_noun pro = _burn_in(wir_r, van, fop, zoc);

          u2_rl_lose(wir_r, fop);
          u2_rl_lose(wir_r, zoc);

          return pro;
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, burn)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    return _burn_in(wir_r, van, sut, u2_nul);
  }

  extern u2_ho_jet 
  j2_mbj(Pt6, ut)[];

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, burn)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ut)[0];   //  total hack

    if ( !(jet_j->sat_s & u2_jet_memo) ) {
      return j2_mcx(Pt6, ut, burn)(wir_r, van, sut);
    }
    else {
      c3_m    fun_m = c3__burn;
      u2_noun pro   = u2_rl_find(wir_r, fun_m, sut);

      if ( u2_none != pro ) {
        return pro;
      }
      else {
        pro = j2_mcx(Pt6, ut, burn)(wir_r, van, sut);

        return u2_rl_save(wir_r, fun_m, sut, pro);
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, burn)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, burn)(wir_r, cor, sut);
    }
  }
