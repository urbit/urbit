/* j/6/ut_repo.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, repo)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_rx(wir_r, sut);

      case c3__noun: 
        return u2_bt(wir_r, c3__fork, 
                            c3__atom, 
                            u2_bt(wir_r, c3__cell, c3__noun, c3__noun));
    }
    else switch ( u2_h(sut) ) {
      default: return u2_rx(wir_r, sut);

      case c3__core: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_bt(wir_r, c3__cell, u2_rx(wir_r, p_sut), c3__noun);
        }
      }
      case c3__cube: {
        p_sut = u2_t(sut);
        {
          if ( u2_yes == u2_stud(p_sut) ) {
            return c3__atom;
          }
          else {
            return u2_bt
              (wir_r, c3__cell,
                      u2_bc(wir_r, c3__cube, u2_rx(wir_r, u2_h(p_sut))),
                      u2_bc(wir_r, c3__cube, u2_rx(wir_r, u2_t(p_sut))));
          }
        }
      }
      case c3__face: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return u2_rx(wir_r, q_sut);
        }
      }
      case c3__hold: {
        if ( u2_no == u2_as_cell(u2_t(sut), &p_sut, &q_sut)) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return j2_mcy(Pit, ut, rest)(wir_r, van, p_sut, q_sut);
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, repo)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_frag(4, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, ut, repo)(wir_r, cor, sut);
    }
  }
