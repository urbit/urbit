/* mill/snap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _snap_cell(): snap [han] into [bar] over (%cell p_tip q_tip).
*/
static u4_type
_snap_cell(u4_milr m,
           u4_type han,
           u4_rail bar,
           u4_type p_tip,
           u4_type q_tip)
{
  u4_lane lane = m->lane;
  u4_noun p_han, q_han;

  if ( u4_b_pq(han, u4_atom_cell, &p_han, &q_han) ) {
    return u4_k_cell
      (lane, 
       _snap_main(m, p_han, _mill_slip(m, u4_noun_2, bar), p_tip),
       _snap_main(m, q_han, _mill_slip(m, u4_noun_3, bar), q_tip));
  }
 
  else if ( u4_b_pq(han, u4_atom_fuse, &p_han, &q_han) ) {
    return _mill_both
      (m,
       _snap_cell(m, p_han, bar, p_tip, q_tip),
       _snap_cell(m, q_han, bar, p_tip, q_tip));
  }

  else if ( u4_b_pq(han, u4_atom_post, &p_han, &q_han) ) {
    return _snap_cell
      (m, _mill_repo(m, p_han, q_han), bar, p_tip, q_tip);
  }

  else {
    /* %bead, %blot, %blur
    ** %cage, %cone, %crib, %cube, %fork
    */
    return han;
  }
}

/* _snap_cage_cage(): snap from cage onto cage, tuples equal.
*/
static u4_noun
_snap_cage_cage(u4_milr m,
                u4_noun p_han, 
                u4_rail bar,
                u4_noun p_tip)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_han) ) {
    return u4_noun_0;
  }
  else {
    u4_type ip_han = u4_ch(p_han);
    u4_noun tp_han = u4_ct(p_han);
    u4_type ip_tip = u4_ch(p_tip);
    u4_noun tp_tip = u4_ct(p_tip);
    u4_rail gad = _mill_slip(m, u4_noun_2, bar);
    u4_rail tog = _mill_slip(m, u4_noun_3, bar);

    return u4_k_cell
      (lane, 
       _snap_main(m, ip_han, gad, ip_tip),
       _snap_cage_cage(m, tp_han, tog, tp_tip));
  }
}

/* _snap_cage_crib(): snap from crib onto cage, tuples equal.
*/
static u4_noun
_snap_cage_crib(u4_milr m,
                u4_noun p_han,
                u4_noun bar,
                u4_noun p_tip)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_han) ) {
    return u4_noun_0;
  }
  else {
    u4_noun ip_han = u4_ch(p_han);
    u4_term pip_han = u4_ch(ip_han);
    u4_type qip_han = u4_ct(ip_han);
    u4_noun tp_han = u4_ct(p_han);
    u4_type ip_tip = u4_ch(p_tip);
    u4_noun tp_tip = u4_ct(p_tip);
    u4_rail gad = _mill_slip(m, u4_noun_2, bar);
    u4_rail tog = _mill_slip(m, u4_noun_3, bar);

    return u4_k_cell
      (m->lane, 
       _snap_main(m, ip_han, gad, qip_tip),
       _snap_cage_crib(m, tp_han, tog, tp_tip));
  }
}

/* _snap_cage(): snap [han] to match [bar] over (%cage p_tip).
*/
static u4_type
_snap_cage(u4_milr m,
           u4_type han,
           u4_rail bar,
           u4_type p_tip)
{
  if ( u4_b_p(han, u4_atom_cage, &p_han) ) {
    if ( u4_log_len(p_han) != u4_log_len(p_tip) ) {
      return han;
    }
    else return _snap_cage_cage(m, p_han, bar, p_tip);
  }

  if ( u4_b_pq(han, u4_atom_cell, &p_han, &q_han) ) {
    if ( 2 == u4_log_len(p_tip) ) {
      return u4_k_qual
        (lane,
         u4_atom_cage,
         _snap_main(m, p_han, 
                       _mill_slip(m, u4_noun_2, bar),
                       u4_ch(p_tip)),
         _snap_main(m, q_han, 
                       _mill_slip(m, u4_noun_3, bar),
                       u4_ch(u4_ct(p_tip))),
         u4_noun_0);
    }
  }

  else if ( u4_b_p(han, u4_atom_crib, &p_han) ) {
    if ( u4_log_len(p_han) != u4_log_len(p_tip) ) {
      return han;
    }
    else return _snap_cage_crib(m, p_han, bar, p_tip);
  }

  else if ( u4_b_pq(han, u4_atom_fuse, &p_han, &q_han) ) {
    return _mill_both
      (m, p_han, _snap_cage(m, q_han, bar, p_tip));
  }

  else if ( u4_b_pq(han, u4_atom_post, &p_han, &q_han) ) {
    return _snap_cage(m, _mill_repo(m, p_han, q_han), bar, p_tip);
  }

  else {
    /* %bead, %blot, %blur
    ** %cone, %cube, %fork
    */
    return han;
  }
}

/* _snap_crib_cage(): snap from cage onto crib, tuples equal.
*/
static u4_noun
_snap_crib_cage(u4_milr m,
                u4_noun p_han,
                u4_noun bar,
                u4_noun p_tip)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_han) ) {
    return u4_noun_0;
  }
  else {
    u4_type ip_han = u4_ch(p_han);
    u4_noun tp_han = u4_ct(p_han);
    u4_noun ip_tip = u4_ch(p_tip);
    u4_term pip_tip = u4_ch(ip_tip);
    u4_type qip_tip = u4_ct(ip_tip);
    u4_noun tp_tip = u4_ct(p_tip);
    u4_rail gad = _mill_slip(m, u4_noun_2, bar);
    u4_rail tog = _mill_slip(m, u4_noun_3, bar);

    return u4_k_cell
      (lane, 
       u4_k_cell(lane, pip_tip, _snap_main(m, ip_han, gad, qip_tip)),
       _snap_crib_cage(m, tp_han, tog, tp_tip));
  }
}

/* _snap_crib_crib(): snap from crib onto crib, tuples equal.
*/
static u4_noun
_snap_crib_crib(u4_milr m,
                u4_noun p_han,
                u4_noun bar,
                u4_noun p_tip)
{
  if ( u4_n_zero(p_han) ) {
    return u4_noun_0;
  }
  else {
    u4_noun ip_han = u4_ch(p_han);
    u4_term pip_han = u4_ch(ip_han);
    u4_type qip_han = u4_ct(ip_han);
    u4_noun tp_han = u4_ct(p_han);
    u4_noun ip_tip = u4_ch(p_tip);
    u4_type qip_tip = u4_ct(ip_tip);
    u4_noun tp_tip = u4_ct(p_tip);
    u4_rail gad = _mill_slip(m, u4_noun_2, bar);
    u4_rail tog = _mill_slip(m, u4_noun_3, bar);

    return u4_k_cell
      (m->lane, 
       u4_k_cell(lane, pip_han, _snap_main(m, qip_han, gad, qip_tip)),
       _snap_crib_crib(m, tp_han, tog, tp_tip));
  }
}

/* _snap_crib_good(): test if crib names match, tuples equal.
*/
static u4_t
_snap_crib_good(u4_milr m,
                u4_noun p_han, 
                u4_noun p_tip)
{
  if ( u4_n_zero(p_han) ) {
    return 1;
  }
  else {
    return u4_n_eq(u4_chh(p_han), u4_chh(p_tip)) &&
           _snap_crib_good(m, u4_ct(p_han), u4_ct(p_tip));
  }
}

/* _snap_crib(): snap [han] to match [bar] over (%crib p_tip).
*/
static u4_type
_snap_crib(u4_milr m,
           u4_type han,
           u4_rail bar,
           u4_type p_tip)
{
  if ( u4_b_p(han, u4_atom_cage, &p_han) ) {
    if ( u4_log_len(p_han) != u4_log_len(p_tip) ) {
      return han;
    }
    else return _snap_crib_cage(m, p_han, bar, p_tip);
  }

  else if ( u4_b_p(han, u4_atom_crib, &p_han) ) {
    if ( (u4_log_len(p_han) != u4_log_len(p_tip)) ||
         !_snap_crib_good(m, p_han, p_tip) )
    { 
      return han;
    } 
    else return _snap_crib_crib(m, p_han, bar, p_tip);
  }

  else if ( u4_b_pq(han, u4_atom_fuse, &p_han, &q_han) ) {
    return _mill_both
      (m, p_han, _snap_cage(m, q_han, bar, p_tip));
  }

  else if ( u4_b_pq(han, u4_atom_post, &p_han, &q_han) ) {
    return _snap_cage(m, _mill_repo(m, p_han, q_han), bar, p_tip);
  }

  else {
    /* %bead, %blot, %blur
    ** %cell, %cone, %cube, %fork
    */
    return han;
  }
}

/* _snap_main(): snap [han] to match [bar] over [tip].  May fail.
**
**    han: source
**    bar: bar on tip
**    tip: destination
*/
static u4_type
_snap_main(u4_milr m,
           u4_type han,
           u4_rail bar,
           u4_type tip)
{
  u4_noun p_tip, q_tip;

  if ( u4_b_p(tip, u4_atom_cage, &p_tip) ) {
    return _snap_cage(m, han, bar, p_tip);
  }

  else if ( u4_b_pq(tip, u4_atom_cell, &p_tip, &q_tip) ) {
    return _snap_cell(m, han, bar, p_tip, q_tip);
  }
        
  else if ( u4_b_p(tip, u4_atom_crib, &p_tip) ) {
    return _snap_crib(m, han, bar, p_tip);
  }

  else if u4_b_pq(tip, u4_atom_fork, &p_tip, &q_tip) {
    if ( _mill_cull(m, bar, p_tip) ) {
      return _snap_main(m, han, bar, q_tip);
    }
    else if ( _mill_cull(m, bar, q_tip) ) {
      return _snap_main(m, han, bar, p_tip);
    }
    else {
      u4_type fid = _snap_main(m, han, bar, p_tip);
      u4_type yex = _snap_main(m, han, bar, q_tip);

      if ( u4_n_eq(fid, yex) ) {
        return fid;
      }
      else return han;
    }
  }
 
  else if ( u4_b_pq(tip, u4_atom_fuse, &p_tip, &q_tip) ) {
    return _snap_main(m, han, u4_k_cell(m->lane, p_tip, bar), q_tip);
  }

  else if ( u4_b_pq(tip, u4_atom_post, &p_tip, &q_tip) ) {
    return _snap_main(m, han, bar, _mill_repo(m, p_tip, q_tip));
  }

  else {
    /* %bead, %blot, %blur
    ** %cone, %cube
    */
    return han;
  }
}

/* _mill_snap(): snap [han] to match [tip].  May fail.
**
**    han: source
**    bar: bar on tip
**    tip: destination
*/
static u4_type
_mill_snap(u4_milr m,
           u4_type han,
           u4_type tip)
{
  if ( _mill_orth(m, han, tip) ) {
    return han;
  } else {
    return _snap_main(m, han, u4_k_cell(m->lane, han, u4_noun_0), tip);
  }
}


/* _snap_cell(): snap a raw cell.
**
**    p_faw: head of source
**    q_faw: tail of source
*/
static u4_type
_snap_cell(u4_milr m,
           u4_type p_faw,
           u4_type q_faw,
           u4_log  bar,
           u4_type tip)
{
  u4_lane lane = m->lane;
  u4_noun p_tip, q_tip;

  if ( u4_b_pq(tip, u4_atom_cell, &p_tip, &q_tip) ) {
    return u4_k_trel
      (lane, u4_atom_cell, 
             _snap_main(m, p_faw, _mill_slip(m, u4_noun_2, bar), p_tip),
             _snap_main(m, q_faw, _mill_slip(m, u4_noun_3, bar), q_tip));
  }
  else if ( u4_b_p(tip, u4_atom_rack, &p_tip) ) {
    u4_noun ip_tip = u4_ch(tip);
    u4_noun tp_tip = u4_ct(tip);
    u4_term pip_tip = u4_ch(ip_tip);
    u4_type qip_tip = u4_ct(ip_tip);

    if ( u4_n_zero(tp_tip) ) {
      return _mill_fail(m, "snap mismatch");
    }


  else {
    /* bead, blot, blur
    ** 
    */
    return u4_k_trel(lane, u4_atom_cell, p_faw, q_faw);
  }
}

/* _mill_snap(): adjust tip for assignment into gan.
*/
u4_type
_mill_snap(u4_milr m,
           u4_type gan,
           u4_type tip)
{
  u4_lane lane = m->lane;

  return _snap_main
    (m, u4_noun_0, gan, u4_noun_0, tip);
}
