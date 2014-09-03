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
    u2_noun rep = u2_cqfu_repo(van, sut);
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

  static u2_noun
  _crop_dext(
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun bix)
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
        u2_noun ret = u2_cqf_bull(p_sut, foz);

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

          if ( u2_yes == u2_cqfu_nest
                              (van, p_ref, u2_no, p_sut) )
          {
            u2_noun foz = _crop_dext(van, q_sut, q_ref, bix);
            u2_noun ret = u2_cqf_cell(p_sut, foz);

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

          if ( u2_yes == u2_cqfu_firm(van, foz, p_sut) ) {
            ret = u2_cqf_cube(p_sut, foz);
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
        u2_noun ret = u2_cqf_face(p_sut, foz);

        u2z(foz);
        return ret;
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun dis = _crop_dext(van, p_sut, ref, bix);
        u2_noun dat = _crop_dext(van, q_sut, ref, bix);
        u2_noun ret = u2_cqf_fork(dis, dat);

        u2z(dis);
        u2z(dat);
        return ret;
      }
      case c3__hold: p_sut = u2t(sut);
      {
        u2_noun hud = u2nc(u2k(sut), u2k(ref));

        if ( u2_yes == u2_cqdi_has(bix, hud) ) {
#         if 0
            u2_noun dun = u2_cqfu_dunq(van, "type", sut);
            u2_noun niz = u2_cqfu_dunq(van, "over", ref);

            u2_ct_push(u2nc(c3__mean, dun));
            u2_ct_push(u2nc(c3__mean, niz));
#         endif

          return u2_cm_error("crop-loop");
        } else {
          u2_noun bux = u2_cqdi_put(bix, hud);
          u2_noun ret = _crop_repo(van, sut, ref, bux);

          u2z(hud);
          u2z(bux);
          return ret;
        }
      }
    }
  }

  static u2_noun
  _crop_sint(
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun bix)
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
        u2_noun rep = u2_cqfu_repo(van, ref);
        u2_noun ret = _crop_dext(van, sut, rep, bix);

        u2z(rep);
        return ret;
      }
    }
  }

  u2_noun
  _cqfu_crop(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun ref)
  {
    return _crop_dext(van, sut, ref, u2_nul);
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_crop(
                       u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_crop(van, sut, ref);
    }
  }

  u2_noun
  u2_cqfu_crop(u2_noun van,
                        u2_noun sut,
                        u2_noun ref)
  {
    c3_m    fun_m = c3__crop;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, ref);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_crop(van, sut, ref);

      return u2_cz_save_2(fun_m, sut, ref, pro);
    }
  }
