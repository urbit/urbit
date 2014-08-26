/* j/6/ut_moot.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  extern u2_ho_jet j2_mbj(Pt6, ut)[];
  static u2_bean _moot_in(u2_noun, u2_noun, u2_noun);

  static u2_bean
  _moot_to(
           u2_noun van,
           u2_noun sut,
           u2_noun gil)
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ut)[1];   // total hack

    if ( !(jet_j->sat_s & u2_jet_memo) ) {
      return _moot_in(van, sut, gil);
    }
    else {
      c3_m    fun_m = c3__moot;
      u2_noun pro   = u2_cz_find(fun_m, sut);

      if ( u2_none != pro ) {
        return pro;
      }
      else {
        pro = _moot_in(van, sut, gil);

        return u2_cz_save(fun_m, sut, pro);
      }
    }
  }

  static u2_bean
  _moot_in(
           u2_noun van,
           u2_noun sut,
           u2_noun gil)
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: {
        return u2_no;
      }
      case c3__void: {
        return u2_yes;
      }
    } else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: p_sut = u2t(sut);
      {
        return u2_no;
      }
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _moot_to(van, q_sut, gil);
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_or(_moot_to(van, p_sut, gil),
                     _moot_to(van, q_sut, gil));
      }
      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _moot_to(van, p_sut, gil);
      }
      case c3__cube: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_no;
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return _moot_to(van, q_sut, gil);
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_and(_moot_to(van, p_sut, gil),
                      _moot_to(van, q_sut, gil));
      }
      case c3__hold: p_sut = u2t(sut);
      {
        if ( (u2_yes == j2_mcc(Pt4, in, has)(gil, sut)) ) {
          return u2_yes;
        }
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(gil, sut);
          u2_noun fop = j2_mcy(Pt6, ut, rest)(van, sut, p_sut);
          u2_noun ret = _moot_to(van, fop, zoc);

          u2z(fop);
          u2z(zoc);

          return ret;
        }
      }
    }
  }


  extern u2_ho_jet
  j2_mbj(Pt6, ut)[];

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, moot)(
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    return _moot_to(van, sut, u2_nul);
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, moot)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcy(Pt6, ut, moot)(cor, sut);
    }
  }

  u2_weak
  j2_mck(Pt6, ut, moot)(
                        u2_noun cor)
  {
    u2_noun sut;

    if ( (u2_none == (sut = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_none;
    } else {
      return u2k(sut);
    }
  }

