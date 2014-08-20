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
        u2_noun pro = j2_mby(Pt6, flip)(ton);

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
          u2_noun lef = j2_mbc(Pt3, peg)(axe, 2);
          u2_noun rit = j2_mbc(Pt3, peg)(axe, 3);
          u2_noun hed = _fish_in(van, p_sut, lef, vit);
          u2_noun tal = _fish_in(van, q_sut, rit, vit);
          u2_noun hob = j2_mby(Pt6, flan)(hed, tal);
          u2_noun vug = j2_mby(Pt6, flan)(hut, hob);

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
          u2_noun pro = j2_mby(Pt6, flor)(hed, tal);

          u2z(hed);
          u2z(tal);

          return pro;
        }
      }
      case c3__hold: {
        p_sut = u2t(sut);
        {
          if ( (u2_yes == j2_mcc(Pt4, in, has)(vit, sut)) ) {
            //  u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "type", sut);
            u2_noun niz = j2_mcy(Pt6, ut, shep)
              (van, "axis", 'd', u2k(axe));

            //  u2_ct_push(u2nc(c3__mean, dun));
            u2_ct_push(u2nc(c3__mean, niz));

            return u2_cm_error("fish-loop");
          } else {
            u2_noun zoc = j2_mcc(Pt4, in, put)(vit, sut);
            u2_noun fop = j2_mcy(Pt6, ut, rest)(van, sut, p_sut);
            u2_noun pro = _fish_in(van, fop, axe, zoc);

            u2z(fop);
            u2z(zoc);

            return pro;
          }
        }
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, fish)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_atom axe)                              //  retain
  {
    return _fish_in(van, sut, axe, u2_nul);
  }


/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fish)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fish)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, axe, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &axe, u2_cv_con, &van, 0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fish)(van, sut, axe);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fish)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "fish");

    if ( u2_none == hoc ) {
      c3_assert(!"register fish");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(axe), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, fish)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fish)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fish)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fish)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, fish)(van, sut, axe);
      }
      else {
        c3_m    fun_m = c3__fish;
        u2_noun pro   = u2_ch_find_2(fun_m, sut, axe);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, fish)(van, sut, axe);

          return u2_ch_save_2(fun_m, sut, axe, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fish)(van, sut, axe);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, fish)(
                        u2_noun cor)
  {
    u2_noun sut, axe, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &axe, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(axe));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fish)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, fish),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, fish), c3__fish,
    },
    { }
  };
