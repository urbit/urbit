/* j/6/crop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _crop_dext(u2_noun, u2_noun, u2_noun, u2_noun);
  static u2_noun
  _crop_sint(u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _crop_repo(
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun bix)
  {
    u2_noun rep = j2_mcy(Pt6, ut, repo)(van, sut);
    u2_noun ret = _crop_dext(van, rep, ref, bix);

    if ( u2_yes == u2_cr_sing(ret, rep) ) {
      if ( c3__void == rep ) {
        return c3__void;
      } else {
        u2z(rep);
        u2z(ret);
        return u2k(sut);
      }
    } else {
      u2z(rep);
      return ret;
    }
  }

  static u2_noun                                                  //  produce
  _crop_dext(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun ref,                                         //  retain
             u2_noun bix)                                         //  retain
  {
    u2_noun p_sut, q_sut, p_ref, q_ref;

    if ( u2_no == u2du(ref) ) {
      switch ( ref ) {
        case c3__void: return u2k(sut);
        case c3__noun: return c3__void;
        default: return u2_cm_bail(c3__fail);
      }
    }
    if ( u2_yes == u2_cr_sing(sut, ref) ) {
      return c3__void;
    }
    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: return _crop_repo(van, sut, ref, bix);
      case c3__void: return c3__void;
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom:
      {
        if ( c3__atom == u2h(ref) ) {
          return c3__void;
        }
        else if ( c3__cell == u2h(ref) ) {
          return u2k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun foz = _crop_dext(van, q_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, bull)(p_sut, foz);

        u2z(foz);
        return ret;
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( c3__atom == u2h(ref) ) {
          return u2k(sut);
        }
        else if ( c3__cell == u2h(ref) ) {
          u2_cx_cell(u2t(ref), &p_ref, &q_ref);

          if ( u2_yes == j2_mcy(Pt6, ut, nest)
                              (van, p_ref, u2_no, p_sut) )
          {
            u2_noun foz = _crop_dext(van, q_sut, q_ref, bix);
            u2_noun ret = j2_mby(Pt6, cell)(p_sut, foz);

            u2z(foz);
            return ret;
          }
          else return u2k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__core:
      {
        if ( (c3__atom == u2h(ref)) || (c3__cell == u2h(ref)) ) {
          return u2k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__cube: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( (c3__cube == u2h(ref)) &&
             (u2_yes == u2_cr_sing(p_sut, u2h(u2t(ref)))) )
        {
          return c3__void;
        }
        else if ( (c3__atom == u2h(ref)) || (c3__cell == u2h(ref)) ) {
          u2_noun foz = _crop_dext(van, q_sut, ref, bix);
          u2_noun ret;

          if ( u2_yes == j2_mcy(Pt6, ut, firm)(van, foz, p_sut) ) {
            ret = j2_mby(Pt6, cube)(p_sut, foz);
          }
          else ret = c3__void;

          u2z(foz);
          return ret;
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun foz = _crop_dext(van, q_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, face)(p_sut, foz);

        u2z(foz);
        return ret;
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun dis = _crop_dext(van, p_sut, ref, bix);
        u2_noun dat = _crop_dext(van, q_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, fork)(dis, dat);

        u2z(dis);
        u2z(dat);
        return ret;
      }
      case c3__hold: p_sut = u2t(sut);
      {
        u2_noun hud = u2nc(u2k(sut), u2k(ref));

        if ( u2_yes == j2_mcc(Pt4, in, has)(bix, hud) ) {
#         if 0
            u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "type", sut);
            u2_noun niz = j2_mcy(Pt6, ut, dunq)(van, "over", ref);

            u2_ct_push(u2nc(c3__mean, dun));
            u2_ct_push(u2nc(c3__mean, niz));
#         endif

          return u2_cm_error("crop-loop");
        } else {
          u2_noun bux = j2_mcc(Pt4, in, put)(bix, hud);
          u2_noun ret = _crop_repo(van, sut, ref, bux);

          u2z(hud);
          u2z(bux);
          return ret;
        }
      }
    }
  }

  static u2_noun                                                  //  produce
  _crop_sint(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun ref,                                         //  retain
             u2_noun bix)                                         //  retain
  {
    u2_noun p_ref, q_ref;

    switch ( u2h(ref) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__bull: u2_cx_cell(u2t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__core:
      case c3__cube: {
        return u2k(sut);
      }
      case c3__face: u2_cx_cell(u2t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__fork: u2_cx_cell(u2t(ref), &p_ref, &q_ref);
      {
        u2_noun hin = _crop_dext(van, sut, p_ref, bix);
        u2_noun ret = _crop_dext(van, hin, q_ref, bix);

        u2z(hin);
        return ret;
      }
      case c3__hold: p_ref = u2t(ref);
      {
        u2_noun rep = j2_mcy(Pt6, ut, repo)(van, ref);
        u2_noun ret = _crop_dext(van, sut, rep, bix);

        u2z(rep);
        return ret;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, crop)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return _crop_dext(van, sut, ref, u2_nul);
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, crop)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, crop)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, crop)(van, sut, ref);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, crop)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    c3_m    fun_m = c3__crop;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, ref);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = j2_mcx(Pt6, ut, crop)(van, sut, ref);

      return u2_cz_save_2(fun_m, sut, ref, pro);
    }
  }

  u2_weak
  j2_mck(Pt6, ut, crop)(u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(ref));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, crop)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, crop),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, crop), c3__crop,
    },
    { }
  };
