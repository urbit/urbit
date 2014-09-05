/* j/6/ut_repo.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u2_noun
  u2_cqfu_repo(
                        u2_noun van,
                        u2_noun sut)
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2k(sut);

      case c3__noun:
        return u2nt(c3__fork,
                            u2nc(c3__atom, u2_blip),
                            u2nt(c3__cell, c3__noun, c3__noun));
    }
    else switch ( u2h(sut) ) {
      default: {
        return u2_cm_error("repo-flat");
      }

      case c3__bull: {
        if ( u2_no == u2_cr_cell(u2t(sut), &p_sut, &q_sut)) {
          return u2_cm_bail(c3__fail);
        } else {
          return u2k(q_sut);
        }
      }
      case c3__core: {
        if ( u2_no == u2_cr_cell(u2t(sut), &p_sut, &q_sut) ) {
          return u2_cm_bail(c3__fail);
        } else {
          return u2nt(c3__cell, c3__noun, u2k(p_sut));
        }
      }
      case c3__cube: {
        if ( u2_no == u2_cr_cell(u2t(sut), &p_sut, &q_sut)) {
          return u2_cm_bail(c3__fail);
        } else {
          return u2k(q_sut);
        }
      }
      case c3__face: {
        if ( u2_no == u2_cr_cell(u2t(sut), &p_sut, &q_sut)) {
          return u2_cm_bail(c3__fail);
        } else {
          return u2k(q_sut);
        }
      }
      case c3__hold: {
        p_sut = u2t(sut);
        return u2_cqfu_rest(van, sut, p_sut);
      }
    }
  }

  u2_noun
  u2_cwfu_repo(
                       u2_noun cor)
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqfu_repo(cor, sut);
    }
  }
