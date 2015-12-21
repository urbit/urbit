/* j/6/heal.c
**
*/
#include "all.h"

/* internals
*/
  static u3_noun
  _heal_as(u3_noun, u3_noun, u3_noun, u3_atom, u3_noun);

  static u3_noun
  _heal_by(u3_noun van,
           u3_noun sut,
           u3_noun qug,
           u3_noun ref)
  {
    u3_noun p_sut, q_sut;

    if ( c3n == u3du(sut) ) {
      return u3m_error("heal-name");
    }
    else switch ( u3h(sut) ) {
      default: return u3m_error("heal-name");

      case c3__core:
      {
        return u3k(ref);
      }
      case c3__bull: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( (c3y == u3du(u3t(qug))) &&
             (c3y == u3r_sing(u3h(p_sut), u3t(u3t(qug))) ) ) {
          return u3k(ref);
        }
        else {
          u3_noun sub = _heal_by(van, q_sut, qug, ref);
          u3_noun ret;

          ret = u3qfu_busk(van, 
                           sub, 
                           u3h(p_sut), 
                           u3h(u3t(p_sut)));

          u3z(sub);
          return ret;
        }
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( (c3n == u3du(u3t(qug))) || 
             (c3y == u3r_sing(p_sut, u3t(u3t(qug)))) ) {
          return u3qf_face(p_sut, ref);
        }
        else return u3m_error("heal-name");
      }
      case c3__hold: {
        u3_noun rep = u3qfu_repo(van, sut);
        u3_noun ret = _heal_by(van, rep, qug, ref);

        u3z(rep);
        return ret;
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dis = _heal_by(van, p_sut, qug, ref);
        u3_noun dat = _heal_by(van, q_sut, qug, ref);
        u3_noun ret = u3qf_fork(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
    }
  }

  static u3_noun
  _heal_to(u3_noun van,
           u3_noun sut,
           u3_noun gil,
           u3_noun qug,
           u3_noun ref,
           u3_atom now,
           u3_atom lat)
  {
    u3_noun p_sut, q_sut;

    if ( c3n == u3du(sut) ) {
      goto repo;
    }
    else switch ( u3h(sut) ) {
      default: goto repo;

      case c3__atom: return c3__void;

      case c3__bull: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun sub = _heal_to(van, q_sut, gil, qug, ref, now, lat);
        u3_noun ret;

        ret = u3qfu_busk
          (van, sub, u3h(p_sut), u3h(u3t(p_sut)));

        u3z(sub);
        return ret;
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun ter, ret;

        if ( 2 == now ) {
          ter = _heal_as(van, p_sut, qug, lat, ref);
          ret = u3qf_cell(ter, q_sut);
        } else {
          ter = _heal_as(van, q_sut, qug, lat, ref);
          ret = u3qf_cell(p_sut, ter);
        }
        u3z(ter);
        return ret;
      }

      case c3__core: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( 3 != now ) {
          return u3m_error("heal-core");
        } else {
          u3_noun ter = _heal_as(van, p_sut, qug, lat, ref);
          u3_noun ret = u3qf_core(ter, q_sut);

          u3z(ter);
          return ret;
        }
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dun = _heal_to(van, q_sut, gil, qug, ref, now, lat);
        u3_noun ret = u3qf_face(p_sut, dun);

        u3z(dun);
        return ret;
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun dis = _heal_to(van, p_sut, gil, qug, ref, now, lat);
        u3_noun dat = _heal_to(van, q_sut, gil, qug, ref, now, lat);
        u3_noun ret = u3qf_fork(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }

      case c3__hold: {
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          return c3__void;
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun rep = u3qfu_repo(van, sut);
          u3_noun ret = _heal_to(van, rep, zoc, qug, ref, now, lat);

          u3z(rep);
          u3z(zoc);

          return ret;
        }
      }
    }

    repo: {
      u3_noun rep = u3qfu_repo(van, sut);
      u3_noun ret = _heal_to(van, rep, gil, qug, ref, now, lat);

      u3z(rep);
      return ret;
    }
  }

  static u3_noun
  _heal_as(u3_noun van,
           u3_noun sut,
           u3_noun qug,
           u3_atom axe,
           u3_noun ref)
  {
    if ( 1 == axe ) {
      if ( u3_nul == qug ) {
        return u3k(ref);
      } else return _heal_by(van, sut, qug, ref);
    }
    else {
      u3_atom now = u3qc_cap(axe);
      u3_atom lat = u3qc_mas(axe);
      u3_noun ret = _heal_to(van, sut, u3_nul, qug, ref, now, lat);

      u3z(lat);
      return ret;
    }
  }

/* functions
*/
  u3_noun
  _cqfu_heal(u3_noun van,
             u3_noun sut,
             u3_noun qug,
             u3_noun axe,
             u3_noun ref)
  {
    return _heal_as(van, sut, qug, axe, ref);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_heal(u3_noun cor)
  {
    u3_noun van, sut, qug, axe, ref;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &qug,
                               u3x_sam_6, &axe,
                               u3x_sam_7, &ref,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3ud(axe)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_heal(van, sut, qug, axe, ref);
    }
  }

  u3_noun
  u3qfu_heal(u3_noun van,
             u3_noun sut,
             u3_noun qug,
             u3_noun axe,
             u3_noun ref)
  {
    return _cqfu_heal(van, sut, qug, axe, ref);
  }

