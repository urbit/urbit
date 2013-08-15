/* mill/type.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _type_book(): load a book.
*/
static u4_noun
_type_book(u4_milr m,
           u4_lump beq)
{
  if ( u4_n_cell(u4_ch(beq)) ) {
    return u4_k_cell
      (m->lane, _type_book(m, u4_ch(beq)), _type_book(m, u4_ct(beq)));
  }
  else {
    return u4_k_cell(m->lane, u4_ch(beq), _mill_gene(m, u4_ct(beq)));
  }
}

/* _type_deco: load a decoration list.
*/
static u4_noun
_type_deco(u4_milr m,
           u4_lump vab)
{
  if ( u4_n_zero(vab) ) {
    return u4_noun_0;
  }
  else {
    u4_noun i_vab = u4_ch(vab);

    return u4_k_cell
      (m->lane, u4_k_cell(m->lane, u4_ch(i_vab), _mill_gene(m, u4_ct(i_vab))),
                _type_deco(m, u4_ct(vab)));
  }
}

/* _type_rack(): load a rack. 
*/

/* _mill_type(): normalize type.
*/
u4_type
_mill_type(u4_milr m,
           u4_lump muf)
{
  u4_lane lane = m->lane;
  u4_noun p_muf, q_muf;

  if ( u4_n_eq(u4_atom_atom, muf) ||
       u4_n_eq(u4_atom_blot, muf) ||
       u4_n_eq(u4_atom_blur, muf) )
  {
    return muf;
  }
  else if ( u4_b_pq(muf, u4_atom_bone, &p_muf, &q_muf) ) {
    if ( u4_n_cell(p_muf) || u4_n_zero(p_muf) || u4_n_eq(u4_noun_1, p_muf) ) {
      goto bad;
    }
    else {
      return u4_k_trel(lane, u4_atom_bone, p_muf, _mill_type(m, q_muf));
    }
  }
  else if ( u4_b_pq(muf, u4_atom_cone, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_cone, _mill_type(m, p_muf), _type_book(m, q_muf));
  }
  else if ( u4_b_p(muf, u4_atom_cube, &p_muf) ) {
    return muf;
  }
  else if ( u4_b_pq(muf, u4_atom_fork, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_fork, _mill_type(m, p_muf), _mill_type(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_fuse, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_fuse, _mill_type(m, p_muf), _mill_type(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_pair, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_pair, _mill_type(m, p_muf), _mill_type(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_post, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_post, _mill_type(m, p_muf), _mill_gene(m, q_muf));
  }
#if 0
  else if ( u4_b_p(muf, u4_atom_rack, &p_muf) ) {
    if ( !u4_n_zero(p_muf) ) {

    return u4_k_cell
      (lane, u4_atom_rack, _type_rack(m, p_muf));
  }
#endif
  else if ( u4_b_pq(muf, u4_atom_skin, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_skin, p_muf, _mill_type(m, q_muf));
  }

  bad: {
    // u4_burp(lane, "type/a", u4_pump_prep(lane, muf));

    return _mill_fail(m, "invalid type");
  }
}
