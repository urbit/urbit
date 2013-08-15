/* mill/x/drag.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _make_book(): make a book.
*/
static u4_noun
_make_book(u4_milr m, u4_noun vab)
{
  u4_lane lane = m->lane;

  // XX: actually balance tree.
  //
  if ( u4_n_zero(vab) ) {
    return u4_k_cell
      (lane, u4_k_trel(lane, u4_noun_0, u4_atom_bail, u4_noun_0),
             u4_noun_0);
  }
  else return u4_log_tupl(lane, vab);
}

/* _drag_book(): bake a book.
*/
static u4_noun
_drag_book(u4_milr m, u4_noun pir, u4_mold tip)
{
  u4_lane lane = m->lane;

  if ( u4_n_cell(u4_ch(pir)) ) {
    return u4_k_cell(lane, _drag_book(m, u4_ch(pir), tip),
                           _drag_book(m, u4_ct(pir), tip));
  }
  else {
    return _mill_bake(m, u4_ct(pir), tip);
  }
}

/* _mill_p_drag()::
*/
u4_mold
_mill_p_drag(u4_milr m, 
             u4_noun vab,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun pir  = _make_book(m, vab);

  return u4_k_trel(lane, u4_atom_poly, tip, pir);
}

/* _mill_b_drag()::
*/
u4_nock
_mill_b_drag(u4_milr m, 
             u4_noun vab,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun pir  = _make_book(m, vab);
  u4_mold heg  = u4_k_trel(lane, u4_atom_mono, tip, pir);
  u4_noun fez  = _drag_book(m, pir, heg);

  return u4_k_cell
          (lane, u4_k_cell(lane, u4_noun_0, u4_noun_1),
                 u4_k_cell(lane, u4_noun_1, fez));
}

/* _mill_m_drag()::
*/
u4_loaf
_mill_m_drag(u4_milr m, 
             u4_noun vab,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun pir  = _make_book(m, vab);
  u4_mold gan  = u4_k_trel(lane, u4_atom_poly, tip, pir);
  u4_mold heg  = u4_k_trel(lane, u4_atom_mono, tip, pir);
  u4_noun fez  = _drag_book(m, pir, heg);

  return u4_k_cell
    (lane, gan,
           u4_k_cell
            (lane, u4_k_cell(lane, u4_noun_0, u4_noun_1),
                   u4_k_cell(lane, u4_noun_1, fez)));
}

