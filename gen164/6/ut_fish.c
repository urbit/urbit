/* j/6/fish.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _fish_in(
           u2_noun van,
           u2_noun sut,
           u2_atom axe,
           u2_noun vit)
  {
    u2_noun p_sut, q_sut;

    if ( u2_yes == u2ud(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: {
        return u2nc(1, 0);
      }
      case c3__void: {
        return u2nc(1, 1);
      }
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: {
        u2_noun ton = u2nt(3, 0, u2k(axe));
        u2_noun pro = u2_cqf_flip(ton);

        u2z(ton);
        return pro;
      }
      case c3__bull: {
        return u2_cm_error("bull-fish");
      }
      case c3__cell: {
        if ( (u2_no == u2_cr_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_cm_bail(c3__fail);
        } else {
          u2_noun hut = u2nt(3, 0, u2k(axe));
          u2_noun lef = u2_cqc_peg(axe, 2);
          u2_noun rit = u2_cqc_peg(axe, 3);
          u2_noun hed = _fish_in(van, p_sut, lef, vit);
          u2_noun tal = _fish_in(van, q_sut, rit, vit);
          u2_noun hob = u2_cqf_flan(hed, tal);
          u2_noun vug = u2_cqf_flan(hut, hob);

          u2z(hob);
          u2z(tal);
          u2z(hed);
          u2z(rit);
          u2z(lef);
          u2z(hut);

          return vug;
        }
      }
      case c3__core: {
        return u2nc(0, 0);
      }
      case c3__cube: {
        if ( (u2_no == u2_cr_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_cm_bail(c3__fail);
        } else {
          return u2nt
            (5,
                    u2nc(1, u2k(p_sut)),
                    u2nc(0, u2k(axe)));
        }
      }
      case c3__face: {
        if ( (u2_no == u2_cr_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_cm_bail(c3__fail);
        } else {
          return _fish_in(van, q_sut, axe, vit);
        }
      }
      case c3__fork: {
        if ( (u2_no == u2_cr_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_cm_bail(c3__fail);
        }
        else {
          u2_noun hed = _fish_in(van, p_sut, axe, vit);
          u2_noun tal = _fish_in(van, q_sut, axe, vit);
          u2_noun pro = u2_cqf_flor(hed, tal);

          u2z(hed);
          u2z(tal);

          return pro;
        }
      }
      case c3__hold: {
        p_sut = u2t(sut);
        {
          if ( (u2_yes == u2_cqdi_has(vit, sut)) ) {
            //  u2_noun dun = u2_cqfu_dunq(van, "type", sut);
            u2_noun niz = u2_cqfu_shep
              (van, "axis", 'd', u2k(axe));

            //  u2_ct_push(u2nc(c3__mean, dun));
            u2_ct_push(u2nc(c3__mean, niz));

            return u2_cm_error("fish-loop");
          } else {
            u2_noun zoc = u2_cqdi_put(vit, sut);
            u2_noun fop = u2_cqfu_rest(van, sut, p_sut);
            u2_noun pro = _fish_in(van, fop, axe, zoc);

            u2z(fop);
            u2z(zoc);

            return pro;
          }
        }
      }
    }
  }
  u2_noun
  _cqfu_fish(
                        u2_noun van,
                        u2_noun sut,
                        u2_atom axe)
  {
    return _fish_in(van, sut, axe, u2_nul);
  }


/* boilerplate
*/
  u2_noun
  u2_cwfu_fish(
                       u2_noun cor)
  {
    u2_noun sut, axe, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &axe, u2_cv_con, &van, 0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_fish(van, sut, axe);
    }
  }

  u2_noun
  u2_cqfu_fish(u2_noun van,
                        u2_noun sut,
                        u2_noun axe)
  {
    c3_m    fun_m = c3__fish;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, axe);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fish(van, sut, axe);

      return u2_cz_save_2(fun_m, sut, axe, pro);
    }
  }
