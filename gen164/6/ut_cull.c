/* j/6/cull.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun
  _cqfu_cull(u2_noun, u2_noun, u2_bean, u2_atom, u2_noun);

  static u2_noun
  _cull_in(
           u2_noun van,
           u2_noun sut,
           u2_bean pol,
           u2_atom axe,
           u2_noun ref,
           u2_noun now,
           u2_noun lat,
           u2_noun vil)
  {
    u2_noun ret, p_sut, q_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: {
        u2_noun pyr = u2_cqfu_repo(van, sut);
        u2_noun yot = _cqfu_cull(van, pyr, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(pyr, yot) ) {
          ret = u2k(sut);
        } else {
          ret = u2k(yot);
        }
        u2z(pyr);
        u2z(yot);
        break;
      }
      case c3__void: {
        ret = c3__void;
      }
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: {
        ret = c3__void;
        break;
      }
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hic = _cqfu_cull
                                    (van, q_sut, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(hic, q_sut) ) {
          ret = u2k(sut);
        } else {
          ret = u2_cqf_bull(p_sut, hic);
        }
        u2z(hic);

        break;
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hub;

        if ( 2 == now ) {
          hub = _cqfu_cull(van, p_sut, pol, lat, ref);
          ret = u2_cqf_cell(hub, q_sut);
        } else {
          hub = _cqfu_cull(van, q_sut, pol, lat, ref);
          ret = u2_cqf_cell(p_sut, hub);
        }
        u2z(hub);
        break;
      }
      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( 3 != now ) {
          ret = u2k(sut);
        } else {
          u2_noun hub = _cqfu_cull
                                   (van, p_sut, pol, lat, ref);

          ret = u2_cqf_core(hub, q_sut);

          u2z(hub);
        }
        break;
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hic = _cqfu_cull
                                    (van, q_sut, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(hic, q_sut) ) {
          ret = u2k(sut);
        } else {
          ret = u2_cqf_face(p_sut, hic);
        }
        u2z(hic);

        break;
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( u2_yes == u2_cqdi_has(vil, sut) ) {
          return c3__void;
        } else {
          u2_noun zoc = u2_cqdi_put(vil, sut);
          u2_noun sin = _cull_in
            (van, p_sut, pol, axe, ref, now, lat, zoc);
          u2_noun dex = _cull_in
            (van, q_sut, pol, axe, ref, now, lat, zoc);

          ret = u2_cqf_fork(sin, dex);
          u2z(sin);
          u2z(dex);
          u2z(zoc);

          return ret;
        }
      }
      case c3__cube:
      case c3__hold:
      {
        u2_noun pyr = u2_cqfu_repo(van, sut);
        u2_noun yot = _cull_in(van, pyr, pol, axe, ref, now, lat, vil);

        if ( u2_yes == u2_cr_sing(pyr, yot) ) {
          if ( c3__void == pyr ) {
            ret = c3__void;
          }
          else ret = u2k(sut);
        } else {
          ret = u2k(yot);
        }
        u2z(pyr);
        u2z(yot);
        break;
      }
    }
    u2z(lat);
    return ret;
  }

  u2_noun
  _cqfu_cull(
                        u2_noun van,
                        u2_noun sut,
                        u2_bean pol,
                        u2_atom axe,
                        u2_noun ref)
  {
    if ( 1 == axe ) {
      if ( u2_yes == pol ) {
        return u2_cqfu_fuse(van, sut, ref);
      } else {
        return u2_cqfu_crop(van, sut, ref);
      }
    } else {
      u2_atom now = u2_cqc_cap(axe);
      u2_atom lat = u2_cqc_mas(axe);

      return _cull_in(van, sut, pol, axe, ref, now, lat, u2_nul);
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_cull(u2_noun cor)
  {
    u2_noun sut, axe, pol, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &pol,
                                u2_cv_sam_6, &axe,
                                u2_cv_sam_7, &ref,
                                u2_cv_con, &van, 0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_cull(van, sut, pol, axe, ref);
    }
  }

  u2_noun
  u2_cqfu_cull(u2_noun van,
                        u2_noun sut,
                        u2_bean pol,
                        u2_noun axe,
                        u2_noun ref)
  {
    c3_m    fun_m = c3__cull;
    u2_noun pro   = u2_cz_find_4(fun_m, sut, pol, axe, ref);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_cull(van, sut, pol, axe, ref);

      return u2_cz_save_4(fun_m, sut, pol, axe, ref, pro);
    }
  }
