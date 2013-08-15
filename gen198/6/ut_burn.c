/* j/6/ut_burn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, burn)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_noun p_sut, q_sut, r_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

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

      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_bc
          (wir_r, j2_mcy(Pt6, ut, burn)(wir_r, van, p_sut),
                  j2_mcy(Pt6, ut, burn)(wir_r, van, q_sut));
      }
      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
                     u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
                     u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);
      {
        return u2_bc
          (wir_r, u2_rx(wir_r, prq_sut),
                  j2_mcy(Pt6, ut, burn)(wir_r, van, p_sut));
      }
      case c3__cube: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_rx(wir_r, p_sut);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return j2_mcy(Pt6, ut, burn)(wir_r, van, q_sut);
      }
      case c3__fine: u2_bi_trel(wir_r, u2_t(sut), &p_sut, &q_sut, &r_sut);
      {
        u2_noun yoc = j2_mcy(Pt6, ut, burn)(wir_r, van, r_sut);

        while ( 1 ) {
          u2_noun feg = j2_mcy(Pt6, ut, bust)(wir_r, van, q_sut, yoc);
          
          if ( p_sut == c3__pure ) {
            if ( u2_yes == u2_sing(yoc, feg) ) {
              u2_rz(wir_r, feg);
              return yoc;
            } else {
              u2_rz(wir_r, yoc);
              yoc = feg;
              continue;
            }
          } 
          else if ( p_sut == c3__very ) {
            if ( u2_yes == feg ) {
              return yoc;
            } else return u2_bl_error(wir_r, "burn-fine");
          }
          else return u2_bl_bail(wir_r, c3__fail);
        }
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return j2_mcy(Pt6, ut, burn)(wir_r, van, p_sut);
      }
      case c3__hold: p_sut = u2_t(sut);
      {
        u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun pro = j2_mcy(Pt6, ut, burn)(wir_r, van, fop);

        u2_rz(wir_r, fop);
        return pro;
      }
    }
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

    if ( u2_none == (sut = u2_frag(u2_cw_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, burn)(wir_r, cor, sut);
    }
  }
