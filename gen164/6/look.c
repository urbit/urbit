/* j/6/look.c
**
** This file is in the public domain.
*/
#include "all.h"


/* internals
*/
  static u2_noun
  _look_in(
           u2_noun cog,
           u2_noun dab,
           u2_atom axe)
  {
    if ( u2_nul == dab ) {
      return u2_nul;
    }
    else {
      u2_noun n_dab, l_dab, r_dab;

      u2_cr_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2du(n_dab) ) {
        return u2_cm_bail(c3__fail);
      }
      else {
        u2_noun pn_dab = u2h(n_dab);
        u2_noun qn_dab = u2t(n_dab);

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          if ( (u2_yes == u2du(qn_dab)) &&
               (u2_yes == u2_cr_sing(cog, pn_dab)) ) {
            return u2nt(u2_nul,
                                u2k(axe),
                                u2k(qn_dab));
          }
          else {
            return u2_nul;
          }
        }
        else if ( (u2_nul == l_dab) ) {
          if ( (u2_yes == u2du(qn_dab)) &&
               (u2_yes == u2_cr_sing(cog, pn_dab)) ) {
            return u2nt(u2_nul,
                                u2_cqc_peg(axe, 2),
                                u2k(qn_dab));
          }
          else {
            if ( u2_yes == u2_cqc_gor(cog, pn_dab) ) {
              return u2_nul;
            }
            else {
              u2_noun pro;

              axe = u2_cqc_peg(axe, 3);
              pro = _look_in(cog, r_dab, axe);
              u2z(axe);
              return pro;
            }
          }
        }
        else if ( (u2_nul == r_dab) ) {
          if ( (u2_yes == u2du(qn_dab)) &&
               (u2_yes == u2_cr_sing(cog, pn_dab)) ) {
            return u2nt(u2_nul,
                                u2_cqc_peg(axe, 2),
                                u2k(qn_dab));
          }
          else {
            if ( u2_yes == u2_cqc_gor(cog, pn_dab) ) {
              u2_noun pro;

              axe = u2_cqc_peg(axe, 3);
              pro = _look_in(cog, l_dab, axe);
              u2z(axe);
              return pro;
            }
            else {
              return u2_nul;
            }
          }
        }
        else {
          if ( (u2_yes == u2du(qn_dab)) &&
               (u2_yes == u2_cr_sing(cog, pn_dab)) ) {
            return u2nt(u2_nul,
                                u2_cqc_peg(axe, 2),
                                u2k(qn_dab));
          }
          else {
            if ( u2_yes == u2_cqc_gor(cog, pn_dab) ) {
              u2_noun pro;

              axe = u2_cqc_peg(axe, 6);
              pro = _look_in(cog, l_dab, axe);
              u2z(axe);
              return pro;
            }
            else {
              u2_noun pro;

              axe = u2_cqc_peg(axe, 7);
              pro = _look_in(cog, r_dab, axe);
              u2z(axe);
              return pro;
            }
          }
        }
      }
    }
  }


/* functions
*/
  u2_noun
  u2_cqf_look(
                    u2_noun cog,
                    u2_noun dab)
  {
    return _look_in(cog, dab, 1);
  }
  u2_noun
  u2_cwf_look(
                   u2_noun cor)
  {
    u2_noun cog, dab;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &cog, u2_cv_sam_3, &dab, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_look(cog, dab);
    }
  }
