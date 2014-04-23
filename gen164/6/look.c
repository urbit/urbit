/* j/6/look.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/
  static u2_noun
  _look_in(u2_wire wir_r,
           u2_noun cog,
           u2_noun dab,
           u2_atom axe)
  {
    if ( u2_nul == dab ) {
      return u2_nul;
    }
    else {
      u2_noun n_dab, l_dab, r_dab;

      u2_as_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2_dust(n_dab) ) {
        return u2_bl_bail(wir_r, c3__fail);
      }
      else {
        u2_noun pn_dab = u2_h(n_dab);
        u2_noun qn_dab = u2_t(n_dab);

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          if ( (u2_yes == u2_dust(qn_dab)) &&
               (u2_yes == u2_sing(cog, pn_dab)) ) {
            return u2_bt(wir_r, u2_nul,
                                u2_rx(wir_r, axe),
                                u2_rx(wir_r, qn_dab));
          }
          else {
            return u2_nul;
          }
        }
        else if ( (u2_nul == l_dab) ) {
          if ( (u2_yes == u2_dust(qn_dab)) &&
               (u2_yes == u2_sing(cog, pn_dab)) ) {
            return u2_bt(wir_r, u2_nul,
                                j2_mbc(Pt3, peg)(wir_r, axe, _2),
                                u2_rx(wir_r, qn_dab));
          }
          else {
            if ( u2_yes == j2_mbc(Pt3, gor)(wir_r, cog, pn_dab) ) {
              return u2_nul;
            }
            else {
              u2_noun pro;

              axe = j2_mbc(Pt3, peg)(wir_r, axe, _3);
              pro = _look_in(wir_r, cog, r_dab, axe);
              u2_rl_lose(wir_r, axe);
              return pro;
            }
          }
        }
        else if ( (u2_nul == r_dab) ) {
          if ( (u2_yes == u2_dust(qn_dab)) &&
               (u2_yes == u2_sing(cog, pn_dab)) ) {
            return u2_bt(wir_r, u2_nul,
                                j2_mbc(Pt3, peg)(wir_r, axe, _2),
                                u2_rx(wir_r, qn_dab));
          }
          else {
            if ( u2_yes == j2_mbc(Pt3, gor)(wir_r, cog, pn_dab) ) {
              u2_noun pro;

              axe = j2_mbc(Pt3, peg)(wir_r, axe, _3);
              pro = _look_in(wir_r, cog, l_dab, axe);
              u2_rl_lose(wir_r, axe);
              return pro;
            }
            else {
              return u2_nul;
            }
          }
        }
        else {
          if ( (u2_yes == u2_dust(qn_dab)) &&
               (u2_yes == u2_sing(cog, pn_dab)) ) {
            return u2_bt(wir_r, u2_nul,
                                j2_mbc(Pt3, peg)(wir_r, axe, _2),
                                u2_rx(wir_r, qn_dab));
          }
          else {
            if ( u2_yes == j2_mbc(Pt3, gor)(wir_r, cog, pn_dab) ) {
              u2_noun pro;

              axe = j2_mbc(Pt3, peg)(wir_r, axe, _6);
              pro = _look_in(wir_r, cog, l_dab, axe);
              u2_rl_lose(wir_r, axe);
              return pro;
            }
            else {
              u2_noun pro;

              axe = j2_mbc(Pt3, peg)(wir_r, axe, _7);
              pro = _look_in(wir_r, cog, r_dab, axe);
              u2_rl_lose(wir_r, axe);
              return pro;
            }
          }
        }
      }
    }
  }


/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, look)(u2_wire wir_r,
                    u2_noun cog,                                  //  retain
                    u2_noun dab)                                  //  retain
  {
    return _look_in(wir_r, cog, dab, _1);
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, look)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun cog, dab;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &cog, u2_cv_sam_3, &dab, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, look)(wir_r, cog, dab);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, look)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, look), Tier6_a, u2_none, u2_none },
    { }
  };
