/* j/6/fuse.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _fuse_in(u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _fuse_repo(
             u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun rep = u3_cqfu_repo(van, sut);
    u3_noun ret = _fuse_in(van, rep, ref, bix);

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
  _fuse_in(
           u3_noun van,
           u3_noun sut,
           u3_noun ref,
           u3_noun bix)
  {
    u3_noun p_sut, q_sut;

    if ( c3y == u3_cr_sing(sut, ref) || (c3__noun == ref) ) {
      return u3k(sut);
    }
    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3_cm_bail(c3__fail);

      case c3__noun:
      {
        return u3k(ref);
      }
      case c3__void:
      {
        return c3__void;
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3_cm_bail(c3__fail);

      case c3__atom:
      {
        if ( c3y == u3du(ref) ) {
          if ( c3__atom == u3h(ref) ) {
            if ( c3y == u3_cqf_fitz(u3t(ref), u3t(sut)) ) {
              return u3k(sut);
            } else return u3k(ref);
          }
          else if ( c3__cell == u3h(ref) ) {
            return c3__void;
          }
        }
        return _fuse_in(van, ref, sut, bix);
      }
      case c3__bull: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun vot = _fuse_in(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_bull(p_sut, vot);

        u3z(vot);
        return ret;
      }
      case c3__cell: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun p_ref, q_ref;

        if ( c3y == u3_cr_pq(ref, c3__cell, &p_ref, &q_ref) ) {
          u3_noun hed = _fuse_in(van, p_sut, p_ref, bix);
          u3_noun tal = _fuse_in(van, q_sut, q_ref, bix);
          u3_noun ret = u3_cqf_cell(hed, tal);

          u3z(hed);
          u3z(tal);
          return ret;
        }
        else return _fuse_in(van, ref, sut, bix);
      }
      case c3__core:
      {
        return _fuse_repo(van, sut, ref, bix);
      }
      case c3__cube: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun foz = _fuse_in(van, q_sut, ref, bix);
        u3_noun ret;

        if ( c3n == u3_cqfu_firm(van, foz, p_sut) ) {
          ret = c3__void;
        } else {
          ret = u3_cqf_cube(p_sut, foz);
        }
        u3z(foz);
        return ret;
      }
      case c3__face: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun vot = _fuse_in(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_face(p_sut, vot);

        u3z(vot);
        return ret;
      }
      case c3__fork: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dis = _fuse_in(van, p_sut, ref, bix);
        u3_noun dat = _fuse_in(van, q_sut, ref, bix);
        u3_noun ret = u3_cqf_fork(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
      case c3__hold: p_sut = u3t(sut);
      {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( c3y == u3_cqdi_has(bix, hud) ) {
          //  u3_noun dun = u3_cqfu_dunq(van, "type", sut);
          //  u3_noun niz = u3_cqfu_dunq(van, "over", ref);

          //  u3_ct_push(u3nc(c3__mean, dun));
          //  u3_ct_push(u3nc(c3__mean, niz));

          return u3_cm_error("fuse-loop");
        } else {
          u3_noun bux = u3_cqdi_put(bix, hud);
          u3_noun ret = _fuse_repo(van, sut, ref, bux);

          u3z(hud);
          u3z(bux);
          return ret;
        }
      }
    }
  }

  u3_noun
  _cqfu_fuse(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun ref)
  {
    return _fuse_in(van, sut, ref, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_fuse(
                       u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam, &ref, u3_cv_con, &van, 0)) ||
         (c3nne == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_fuse(van, sut, ref);
    }
  }

  u3_noun
  u3_cqfu_fuse(u3_noun van,
                        u3_noun sut,
                        u3_noun ref)
  {
    c3_m    fun_m = c3__fuse;
    u3_noun pro   = u3_cz_find_2(fun_m, sut, ref);

    if ( c3nne != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fuse(van, sut, ref);

      return u3_cz_save_2(fun_m, sut, ref, pro);
    }
  }
