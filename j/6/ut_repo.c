/* j/6/ut_repo.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u3_noun
  u3_cqfu_repo(
                        u3_noun van,
                        u3_noun sut)
  {
    u3_noun p_sut, q_sut;

    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3k(sut);

      case c3__noun:
        return u3nt(c3__fork,
                            u3nc(c3__atom, u3_blip),
                            u3nt(c3__cell, c3__noun, c3__noun));
    }
    else switch ( u3h(sut) ) {
      default: {
        return u3m_error("repo-flat");
      }

      case c3__bull: {
        if ( c3n == u3r_cell(u3t(sut), &p_sut, &q_sut)) {
          return u3m_bail(c3__fail);
        } else {
          return u3k(q_sut);
        }
      }
      case c3__core: {
        if ( c3n == u3r_cell(u3t(sut), &p_sut, &q_sut) ) {
          return u3m_bail(c3__fail);
        } else {
          return u3nt(c3__cell, c3__noun, u3k(p_sut));
        }
      }
      case c3__cube: {
        if ( c3n == u3r_cell(u3t(sut), &p_sut, &q_sut)) {
          return u3m_bail(c3__fail);
        } else {
          return u3k(q_sut);
        }
      }
      case c3__face: {
        if ( c3n == u3r_cell(u3t(sut), &p_sut, &q_sut)) {
          return u3m_bail(c3__fail);
        } else {
          return u3k(q_sut);
        }
      }
      case c3__hold: {
        p_sut = u3t(sut);
        return u3_cqfu_rest(van, sut, p_sut);
      }
    }
  }

  u3_noun
  u3_cwfu_repo(
                       u3_noun cor)
  {
    u3_noun sut;

    if ( u3_none == (sut = u3r_at(u3v_sam, cor)) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3_cqfu_repo(cor, sut);
    }
  }
