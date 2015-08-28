/* j/6/crop.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _crop_dext(u3_noun, u3_noun, u3_noun, u3_noun);
  static u3_noun
  _crop_sint(u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _crop_repo(u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun rep = u3qfu_repo(van, sut);
    u3_noun ret = _crop_dext(van, rep, ref, bix);

    if ( c3y == u3r_sing(ret, rep) ) {
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
  _crop_dext(u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun p_sut, q_sut, p_ref, q_ref;

    if ( c3n == u3du(ref) ) {
      switch ( ref ) {
        case c3__void: return u3k(sut);
        case c3__noun: return c3__void;
        default: return u3m_bail(c3__fail);
      }
    }
    if ( c3y == u3r_sing(sut, ref) ) {
      return c3__void;
    }
    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: return _crop_repo(van, sut, ref, bix);
      case c3__void: return c3__void;
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

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
      case c3__bull: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun foz = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3qf_bull(p_sut, foz);

        u3z(foz);
        return ret;
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( c3__atom == u3h(ref) ) {
          return u3k(sut);
        }
        else if ( c3__cell == u3h(ref) ) {
          u3x_cell(u3t(ref), &p_ref, &q_ref);

          if ( c3y == u3qfu_nest(van, p_ref, c3n, p_sut) )
          {
            u3_noun foz = _crop_dext(van, q_sut, q_ref, bix);
            u3_noun ret = u3qf_cell(p_sut, foz);

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
      case c3__cube: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( (c3__cube == u3h(ref)) &&
             (c3y == u3r_sing(p_sut, u3h(u3t(ref)))) )
        {
          return c3__void;
        }
        else if ( (c3__atom == u3h(ref)) || (c3__cell == u3h(ref)) ) {
          u3_noun foz = _crop_dext(van, q_sut, ref, bix);
          u3_noun ret;

          if ( c3y == u3qfu_firm(van, foz, p_sut) ) {
            ret = u3qf_cube(p_sut, foz);
          }
          else ret = c3__void;

          u3z(foz);
          return ret;
        }
        else return _crop_sint(van, sut, ref, bix);
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun foz = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3qf_face(p_sut, foz);

        u3z(foz);
        return ret;
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dis = _crop_dext(van, p_sut, ref, bix);
        u3_noun dat = _crop_dext(van, q_sut, ref, bix);
        u3_noun ret = u3qf_fork(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
      case c3__hold: p_sut = u3t(sut);
      {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( c3y == u3qdi_has(bix, hud) ) {
#         if 0
            u3_noun dun = u3qfu_dunq(van, "type", sut);
            u3_noun niz = u3qfu_dunq(van, "over", ref);

            u3t_push(u3nc(c3__mean, dun));
            u3t_push(u3nc(c3__mean, niz));
#         endif

          return u3m_error("crop-loop");
        } else {
          u3_noun bux = u3qdi_put(bix, hud);
          u3_noun ret = _crop_repo(van, sut, ref, bux);

          u3z(hud);
          u3z(bux);
          return ret;
        }
      }
    }
  }

  static u3_noun
  _crop_sint(u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun p_ref, q_ref;

    switch ( u3h(ref) ) {
      default: return u3m_bail(c3__fail);

      case c3__bull: u3x_cell(u3t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__core:
      case c3__cube: {
        return u3k(sut);
      }
      case c3__face: u3x_cell(u3t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(van, sut, q_ref, bix);
      }
      case c3__fork: u3x_cell(u3t(ref), &p_ref, &q_ref);
      {
        u3_noun hin = _crop_dext(van, sut, p_ref, bix);
        u3_noun ret = _crop_dext(van, hin, q_ref, bix);

        u3z(hin);
        return ret;
      }
      case c3__hold: p_ref = u3t(ref);
      {
        u3_noun rep = u3qfu_repo(van, ref);
        u3_noun ret = _crop_dext(van, sut, rep, bix);

        u3z(rep);
        return ret;
      }
    }
  }

  u3_noun
  _cqfu_crop(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    return _crop_dext(van, sut, ref, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_crop(u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &ref, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_crop(van, sut, ref);
    }
  }

  u3_noun
  u3qfu_crop(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    c3_m    fun_m = c3__crop;
    u3_noun pro   = u3z_find_2(fun_m, sut, ref);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_crop(van, sut, ref);

      return u3z_save_2(fun_m, sut, ref, pro);
    }
  }
