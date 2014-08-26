/* j/6/ut_burn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _burn_in(
           u2_noun van,
           u2_noun sut,
           u2_noun gil)
  {
    u2_noun p_sut, q_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: return 0;
      case c3__void: {
        return u2_cm_error("burn-void");
      }
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: return 0;
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _burn_in(van, q_sut, gil);
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2nc
          (_burn_in(van, p_sut, gil),
                  _burn_in(van, q_sut, gil));
      }
      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
                     u2_cx_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
                     u2_cx_cell(rq_sut, &prq_sut, &qrq_sut);
      {
        return u2nc
          (u2k(prq_sut),
                  _burn_in(van, p_sut, gil));
      }
      case c3__cube: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2k(p_sut);
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _burn_in(van, q_sut, gil);
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _burn_in(van, p_sut, gil);
      }
      case c3__hold: p_sut = u2t(sut);
      {
        if ( (u2_yes == j2_mcc(Pt4, in, has)(gil, sut)) ) {
          u2_noun sux = j2_mcy(Pt6, ut, dunq)(van, "type", sut);

          u2_ct_push(u2nc(c3__mean, sux));
          return u2_cm_error("burn-loop");
        }
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(gil, sut);
          u2_noun fop = j2_mcy(Pt6, ut, repo)(van, sut);
          u2_noun pro = _burn_in(van, fop, zoc);

          u2z(fop);
          u2z(zoc);

          return pro;
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, burn)(
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    return _burn_in(van, sut, u2_nul);
  }

  extern u2_ho_jet
  j2_mbj(Pt6, ut)[];

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, burn)(
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ut)[0];   //  total hack

    if ( !(jet_j->sat_s & u2_jet_memo) ) {
      return j2_mcx(Pt6, ut, burn)(van, sut);
    }
    else {
      c3_m    fun_m = c3__burn;
      u2_noun pro   = u2_cz_find(fun_m, sut);

      if ( u2_none != pro ) {
        return pro;
      }
      else {
        pro = j2_mcx(Pt6, ut, burn)(van, sut);

        return u2_cz_save(fun_m, sut, pro);
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, burn)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcy(Pt6, ut, burn)(cor, sut);
    }
  }
