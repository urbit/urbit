/* j/6/fish.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _fish_in(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_atom axe,
           u2_noun vit)
  {
    u2_noun p_sut, q_sut;

    if ( u2_yes == u2_stud(sut) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: {
        return u2_bc(wir_r, _1, _0);
      }
      case c3__void: {
        return u2_bc(wir_r, _1, _1);
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: {
        u2_noun ton = u2_bt(wir_r, _3, _0, u2_rx(wir_r, axe));
        u2_noun pro = j2_mby(Pt6, flip)(wir_r, ton);

        u2_rl_lose(wir_r, ton);
        return pro;
      }
      case c3__bull: {
        return u2_bl_error(wir_r, "bull-fish");
      }
      case c3__cell: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun hut = u2_bt(wir_r, _3, _0, u2_rx(wir_r, axe));
          u2_noun lef = j2_mbc(Pt3, peg)(wir_r, axe, _2);
          u2_noun rit = j2_mbc(Pt3, peg)(wir_r, axe, _3);
          u2_noun hed = _fish_in(wir_r, van, p_sut, lef, vit);
          u2_noun tal = _fish_in(wir_r, van, q_sut, rit, vit);
          u2_noun hob = j2_mby(Pt6, flan)(wir_r, hed, tal);
          u2_noun vug = j2_mby(Pt6, flan)(wir_r, hut, hob);

          u2_rl_lose(wir_r, hob);
          u2_rl_lose(wir_r, tal);
          u2_rl_lose(wir_r, hed);
          u2_rl_lose(wir_r, rit);
          u2_rl_lose(wir_r, lef);
          u2_rl_lose(wir_r, hut);

          return vug;
        }
      }
      case c3__core: {
        return u2_bc(wir_r, _0, _0);
      }
      case c3__cube: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_bt
            (wir_r, _5, 
                    u2_bc(wir_r, _1, u2_rx(wir_r, p_sut)),
                    u2_bc(wir_r, _0, u2_rx(wir_r, axe)));
        }
      }
      case c3__face: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else { 
          return _fish_in(wir_r, van, q_sut, axe, vit);
        }
      }
      case c3__fork: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          u2_noun hed = _fish_in(wir_r, van, p_sut, axe, vit);
          u2_noun tal = _fish_in(wir_r, van, q_sut, axe, vit);
          u2_noun pro = j2_mby(Pt6, flor)(wir_r, hed, tal);

          u2_rl_lose(wir_r, hed);
          u2_rl_lose(wir_r, tal);

          return pro;
        }
      } 
      case c3__hold: {
        p_sut = u2_t(sut);
        {
          if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, vit, sut)) ) {
            //  u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "type", sut);
            u2_noun niz = j2_mcy(Pt6, ut, shep)
              (wir_r, van, "axis", 'd', u2_rx(wir_r, axe));

            //  u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
            u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

            return u2_bl_error(wir_r, "fish-loop");
          } else {
            u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, vit, sut);
            u2_noun fop = j2_mcy(Pt6, ut, rest)(wir_r, van, sut, p_sut);
            u2_noun pro = _fish_in(wir_r, van, fop, axe, zoc);

            u2_rl_lose(wir_r, fop);
            u2_rl_lose(wir_r, zoc);

            return pro;
          }
        }
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, fish)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_atom axe)                              //  retain
  {
    return _fish_in(wir_r, van, sut, axe, u2_nul);
  }


/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, fish)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fish)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, axe, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &axe, u2_cv_con, &van, 0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fish)(wir_r, van, sut, axe);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fish)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun axe)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "fish");

    if ( u2_none == hoc ) {
      c3_assert(!"register fish");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, axe), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, fish)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fish)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fish)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fish)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, fish)(wir_r, van, sut, axe);
      }
      else {
        c3_m    fun_m = c3__fish;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, axe);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, fish)(wir_r, van, sut, axe);

          return u2_rl_save_cell(wir_r, fun_m, sut, axe, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fish)(wir_r, van, sut, axe);
      fol = u2_h(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, fish)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, axe, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &axe, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, axe));
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
