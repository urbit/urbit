/* j/6/ut_burn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, burn)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_noun p_sut, q_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: return _0;
      case c3__void: return u2_bl_bail(wir_r, c3__fail);
      case c3__noun: return _0;
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return u2_bc
          (wir_r, j2_mcy(Pit, ut, burn)(wir_r, van, p_sut),
                  j2_mcy(Pit, ut, burn)(wir_r, van, q_sut));
      }
      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
                     u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
                     u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);
      {
        if ( u2_no == u2_dust(prq_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_bc
            (wir_r, j2_mcy(Pit, ut, burn)(wir_r, van, p_sut),
                    u2_rx(wir_r, prq_sut));
        }
      }
      case c3__cube: p_sut = u2_t(sut);
      {
        return u2_rx(wir_r, p_sut);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        return j2_mcy(Pit, ut, burn)(wir_r, van, q_sut);
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun heb = j2_mcy(Pit, ut, burn)(wir_r, van, p_sut);
        u2_noun nar = j2_mcy(Pit, ut, burn)(wir_r, van, q_sut);

        if ( u2_no == u2_sing(heb, nar) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        u2_rl_lose(wir_r, nar);
        return heb;
      }
      case c3__hold: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun rep = j2_mcy(Pit, ut, repo)(wir_r, van, sut);
        u2_noun fos = j2_mcy(Pit, ut, burn)(wir_r, van, rep);

        u2_rl_lose(wir_r, rep);
        return fos;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, burn)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, ut, burn)(wir_r, cor, sut);
    }
  }
