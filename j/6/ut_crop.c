/* j/6/crop.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _crop_dext(u3_noun, u3_noun, u3_noun, u3_noun);
  static u3_noun
  _crop_sint(u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _crop_repo(
             u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun rep = u3_cqfu_repo(van, sut);
    u3_noun ret = _crop_dext(van, rep, ref, bix);

    if ( c3y == u3_cr_sing(ret, rep) ) {
      if ( c3__void == rep ) {
        return c3__void;
      } else {
        u3z(rep);
        u3z(ret);
        return u3k(sut);
      }
    } else {
      u3z(rep);
      return ret;
    }
  }

  static u3_noun
  _crop_dext(
             u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun p_sut, q_sut, p_ref, q_ref;

    if ( c3n == u3du(ref) ) {
      switch ( ref ) {
        case c3__void: return u3k(sut);
        case c3__noun: return c3__void;
        default: return u3_cm_bail(c3__fail);
      }
    }
    if ( c3y == u3_cr_sing(sut, ref) ) {
      return c3__void;
    }
    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3_cm_bail(c3__fail);

      case c3__noun: return _crop_repo(van, sut, ref, bix);
      case c3__void: return c3__void;
    }
    else switch ( u3h(sut) ) {
      default: return u3_cm_bail(c3__fail);

      case c3__atom:
      {
        if ( c3__atom == u3h(ref) ) {
          return c3__void;
        }
        else if ( c3__cell == u3h(ref) ) {
          return u3k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__bull: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun foz = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_bull(p_sut, foz);

        u3z(foz);
        return ret;
      }
      case c3__cell: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( c3__atom == u3h(ref) ) {
          return u3k(sut);
        }
        else if ( c3__cell == u3h(ref) ) {
          u3_cx_cell(u3t(ref), &p_ref, &q_ref);

          if ( c3y == u3_cqfu_nest
                              (van, p_ref, c3n, p_sut) )
          {
            u3_noun foz = _crop_dext(van, q_sut, q_ref, bix);
            u3_noun ret = u3_cqf_cell(p_sut, foz);

            u3z(foz);
            return ret;
          }
          else return u3k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__core:
      {
        if ( (c3__atom == u3h(ref)) || (c3__cell == u3h(ref)) ) {
          return u3k(sut);
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__cube: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( (c3__cube == u3h(ref)) &&
             (c3y == u3_cr_sing(p_sut, u3h(u3t(ref)))) )
        {
          return c3__void;
        }
        else if ( (c3__atom == u3h(ref)) || (c3__cell == u3h(ref)) ) {
          u3_noun foz = _crop_dext(van, q_sut, ref, bix);
          u3_noun ret;

          if ( c3y == u3_cqfu_firm(van, foz, p_sut) ) {
            ret = u3_cqf_cube(p_sut, foz);
          }
          else ret = c3__void;

          u3z(foz);
          return ret;
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__face: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun foz = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_face(p_sut, foz);

        u3z(foz);
        return ret;
      }
      case c3__fork: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dis = _crop_dext(van, p_sut, ref, bix);
        u3_noun dat = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_fork(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
      case c3__hold: p_sut = u3t(sut);
      {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( c3y == u3_cqdi_has(bix, hud) ) {
#         if 0
            u3_noun dun = u3_cqfu_dunq(van, "type", sut);
            u3_noun niz = u3_cqfu_dunq(van, "over", ref);

            u3_ct_push(u3nc(c3__mean, dun));
            u3_ct_push(u3nc(c3__mean, niz));
#         endif

          return u3_cm_error("crop-loop");
        } else {
          u3_noun bux = u3_cqdi_put(bix, hud);
          u3_noun ret = _crop_repo(van, sut, ref, bux);

          u3z(hud);
          u3z(bux);
          return ret;
        }
      }
    }
  }

  static u3_noun
  _crop_sint(
             u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun p_ref, q_ref;

    switch ( u3h(ref) ) {
      default: return u3_cm_bail(c3__fail);

      case c3__bull: u3_cx_cell(u3t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__core:
      case c3__cube: {
        return u3k(sut);
      }
      case c3__face: u3_cx_cell(u3t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__fork: u3_cx_cell(u3t(ref), &p_ref, &q_ref);
      {
        u3_noun hin = _crop_dext(van, sut, p_ref, bix);
        u3_noun ret = _crop_dext(van, hin, q_ref, bix);

        u3z(hin);
        return ret;
      }
      case c3__hold: p_ref = u3t(ref);
      {
        u3_noun rep = u3_cqfu_repo(van, ref);
        u3_noun ret = _crop_dext(van, sut, rep, bix);

        u3z(rep);
        return ret;
      }
    }
  }

  u3_noun
  _cqfu_crop(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun ref)
  {
    return _crop_dext(van, sut, ref, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_crop(
                       u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam, &ref, u3_cv_con, &van, 0)) ||
         (c3nne == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_crop(van, sut, ref);
    }
  }

  u3_noun
  u3_cqfu_crop(u3_noun van,
                        u3_noun sut,
                        u3_noun ref)
  {
    c3_m    fun_m = c3__crop;
    u3_noun pro   = u3_cz_find_2(fun_m, sut, ref);

    if ( c3nne != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_crop(van, sut, ref);

      return u3_cz_save_2(fun_m, sut, ref, pro);
    }
  }
