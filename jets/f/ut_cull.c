/* j/6/cull.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_cull(u3_noun, u3_noun, u3_noun, u3_atom, u3_noun);

  static u3_noun
  _cull_in(u3_noun van,
           u3_noun sut,
           u3_noun pol,
           u3_atom axe,
           u3_noun ref,
           u3_noun now,
           u3_noun lat,
           u3_noun vil)
  {
    u3_noun ret, p_sut, q_sut;

    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: {
        u3_noun pyr = u3qfu_repo(van, sut);
        u3_noun yot = _cqfu_cull(van, pyr, pol, axe, ref);

        if ( c3y == u3r_sing(pyr, yot) ) {
          ret = u3k(sut);
        } else {
          ret = u3k(yot);
        }
        u3z(pyr);
        u3z(yot);
        break;
      }
      case c3__void: {
        ret = c3__void;
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: {
        ret = c3__void;
        break;
      }
      case c3__bull: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun hic = _cqfu_cull(van, q_sut, pol, axe, ref);

        if ( c3y == u3r_sing(hic, q_sut) ) {
          ret = u3k(sut);
        } else {
          ret = u3qf_bull(p_sut, hic);
        }
        u3z(hic);

        break;
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun hub;

        if ( 2 == now ) {
          hub = _cqfu_cull(van, p_sut, pol, lat, ref);
          ret = u3qf_cell(hub, q_sut);
        } else {
          hub = _cqfu_cull(van, q_sut, pol, lat, ref);
          ret = u3qf_cell(p_sut, hub);
        }
        u3z(hub);
        break;
      }
      case c3__core: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( 3 != now ) {
          ret = u3k(sut);
        } else {
          u3_noun hub = _cqfu_cull(van, p_sut, pol, lat, ref);

          ret = u3qf_core(hub, q_sut);

          u3z(hub);
        }
        break;
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun hic = _cqfu_cull(van, q_sut, pol, axe, ref);

        if ( c3y == u3r_sing(hic, q_sut) ) {
          ret = u3k(sut);
        } else {
          ret = u3qf_face(p_sut, hic);
        }
        u3z(hic);

        break;
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( c3y == u3qdi_has(vil, sut) ) {
          return c3__void;
        } else {
          u3_noun zoc = u3qdi_put(vil, sut);
          u3_noun sin = _cull_in(van, p_sut, pol, axe, ref, now, lat, zoc);
          u3_noun dex = _cull_in(van, q_sut, pol, axe, ref, now, lat, zoc);

          ret = u3qf_fork(sin, dex);
          u3z(sin);
          u3z(dex);
          u3z(zoc);

          return ret;
        }
      }
      case c3__cube:
      case c3__hold:
      {
        u3_noun pyr = u3qfu_repo(van, sut);
        u3_noun yot = _cull_in(van, pyr, pol, axe, ref, now, lat, vil);

        if ( c3y == u3r_sing(pyr, yot) ) {
          if ( c3__void == pyr ) {
            ret = c3__void;
          }
          else ret = u3k(sut);
        } else {
          ret = u3k(yot);
        }
        u3z(pyr);
        u3z(yot);
        break;
      }
    }
    u3z(lat);
    return ret;
  }

  u3_noun
  _cqfu_cull(u3_noun van,
             u3_noun sut,
             u3_noun pol,
             u3_atom axe,
             u3_noun ref)
  {
    if ( 1 == axe ) {
      if ( c3y == pol ) {
        return u3qfu_fuse(van, sut, ref);
      } else {
        return u3qfu_crop(van, sut, ref);
      }
    } else {
      u3_atom now = u3qc_cap(axe);
      u3_atom lat = u3qc_mas(axe);

      return _cull_in(van, sut, pol, axe, ref, now, lat, u3_nul);
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_cull(u3_noun cor)
  {
    u3_noun sut, axe, pol, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &pol,
                               u3x_sam_6, &axe,
                               u3x_sam_7, &ref,
                               u3x_con, &van, 0)) ||
         (c3n == u3ud(axe)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_cull(van, sut, pol, axe, ref);
    }
  }

  u3_noun
  u3qfu_cull(u3_noun van,
             u3_noun sut,
             u3_noun pol,
             u3_noun axe,
             u3_noun ref)
  {
    c3_m    fun_m = c3__cull + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_4(fun_m, sut, pol, axe, ref);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_cull(van, sut, pol, axe, ref);

      return u3z_save_4(fun_m, sut, pol, axe, ref, pro);
    }
  }
