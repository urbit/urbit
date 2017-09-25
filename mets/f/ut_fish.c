/* j/6/fish.c
**
*/
#include "all.h"

/* functions
*/
  static u3_noun
  _fish_in(u3_noun, u3_noun, u3_atom, u3_noun);

  static u3_noun
  _fish_fork(u3_noun van,
             u3_noun p_sut,
             u3_atom axe,
             u3_noun vit)
  {
    if ( u3_nul == p_sut ) {
      return u3nc(1, 1);
    } 
    else {
      u3_noun hed = _fish_in(van, u3h(p_sut), axe, vit);
      u3_noun tal = _fish_fork(van, u3t(p_sut), axe, vit);
      u3_noun pro = u3gf_flor(hed, tal);

      u3z(hed);
      u3z(tal);

      return pro;
    }
  }

  static u3_noun
  _fish_in(u3_noun van,
           u3_noun sut,
           u3_atom axe,
           u3_noun vit)
  {
    u3_noun p_sut, q_sut;

    if ( c3y == u3ud(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: {
        return u3nc(1, 0);
      }
      case c3__void: {
        return u3nc(1, 1);
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( c3n == u3du(q_sut) ) {
            u3_noun ton = u3nt(3, 0, u3k(axe));
            u3_noun pro = u3gf_flip(ton);

            u3z(ton);
            return pro;
          } else {
            return u3nt(5,
                        u3nc(1, u3k(u3t(q_sut))),
                        u3nc(0, u3k(axe)));
          }
        }
      }
      case c3__cell: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          u3_noun hut = u3nt(3, 0, u3k(axe));
          u3_noun lef = u3gc_peg(axe, 2);
          u3_noun rit = u3gc_peg(axe, 3);
          u3_noun hed = _fish_in(van, p_sut, lef, vit);
          u3_noun tal = _fish_in(van, q_sut, rit, vit);
          u3_noun hob = u3gf_flan(hed, tal);
          u3_noun vug = u3gf_flan(hut, hob);

          u3z(hob);
          u3z(tal);
          u3z(hed);
          u3z(rit);
          u3z(lef);
          u3z(hut);

          return vug;
        }
      }
      case c3__core: {
        return u3nc(0, 0);
      }
      case c3__help:
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          return _fish_in(van, q_sut, axe, vit);
        }
      }
      case c3__fork: p_sut = u3t(sut);
      {
        u3_noun yed = u3gdi_tap(p_sut);
        u3_noun ret = _fish_fork(van, yed, axe, vit);

        u3z(yed);
        return ret;
      }
      case c3__hold: {
        p_sut = u3t(sut);
        {
          if ( (c3y == u3gdi_has(vit, sut)) ) {
            //  u3_noun dun = u3gfu_dunq(van, "type", sut);
            u3_noun niz = u3gfu_shep
              (van, "axis", 'd', u3k(axe));

            //  u3t_push(u3nc(c3__mean, dun));
            u3t_push(u3nc(c3__mean, niz));

            return u3m_error("fish-loop");
          } else {
            u3_noun zoc = u3gdi_put(vit, sut);
            u3_noun fop = u3gfu_repo(van, sut);
            u3_noun pro = _fish_in(van, fop, axe, zoc);

            u3z(fop);
            u3z(zoc);

            return pro;
          }
        }
      }
    }
  }

  static u3_noun
  _cqfu_fish(u3_noun van,
             u3_noun sut,
             u3_atom axe)
  {
    return _fish_in(van, sut, axe, u3_nul);
  }


/* boilerplate
*/
  u3_noun
  u3yfu_fish(u3_noun cor)
  {
    u3_noun sut, axe, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &axe, u3x_con, &van, 0)) ||
         (c3n == u3ud(axe)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fish(van, sut, axe);
    }
  }

  u3_noun
  u3gfu_fish(u3_noun van,
             u3_noun sut,
             u3_noun axe)
  {
    c3_m    fun_m = 144 + c3__fish + ((!!u3r_at(u3gfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_2(fun_m, sut, axe);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fish(van, sut, axe);

      return u3z_save_2(fun_m, sut, axe, pro);
    }
  }
