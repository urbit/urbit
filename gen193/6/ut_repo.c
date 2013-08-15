/* j/6/ut_repo.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, repo)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_noun p_sut, q_sut, r_sut;

    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_rx(wir_r, sut);

      case c3__noun: 
        return u2_bt(wir_r, c3__fork, 
                            u2_bc(wir_r, c3__atom, u2_blip),
                            u2_bt(wir_r, c3__cell, c3__noun, c3__noun));
    }
    else switch ( u2_h(sut) ) {
      default: {
        return u2_bl_error(wir_r, "repo-flat");
      }

      case c3__core: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_bt(wir_r, c3__cell, c3__noun, u2_rx(wir_r, p_sut));
        }
      }
      case c3__cube: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_rx(wir_r, q_sut);
        }
      }
      case c3__face: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_rx(wir_r, q_sut);
        }
      }
      case c3__fine: {
        if ( u2_no == u2_as_trel(u2_t(sut), &p_sut, &q_sut, &r_sut)) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_rx(wir_r, r_sut);
        }
      }
      case c3__hold: {
        p_sut = u2_t(sut);
        return j2_mcy(Pt6, ut, rest)(wir_r, van, sut, p_sut);
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, repo)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_frag(u2_cv_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, repo)(wir_r, cor, sut);
    }
  }
