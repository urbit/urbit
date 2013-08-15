/* mill/dump.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _dump_count()::
*/
static u4_atom
_dump_count(u4_lane lane, u4_noun q_daf)
{
  if ( u4_n_atom(u4_ch(q_daf)) ) {
    return u4_noun_1;
  }
  else {
    return u4_op_add
      (lane, _dump_count(lane, u4_ch(q_daf)), 
             _dump_count(lane, u4_ct(q_daf)));
  }
}

/* _dump_weight()::
*/
static u4_atom
_dump_weight(u4_lane lane, u4_noun gub)
{
  if ( u4_n_atom(gub) ) {
    return u4_noun_1;
  }
  else {
    return u4_op_add
      (lane, _dump_weight(lane, u4_ch(gub)), 
             _dump_weight(lane, u4_ct(gub)));
  }
}

#if 0
/* _dump_all():
*/
static u4_prep
_dump_all(u4_milr m, u4_atom daf)
{
  u4_lane lane = m->lane;
  u4_noun fos  = u4_ch(daf);
  u4_noun bir  = u4_ch(u4_ct(daf));
  u4_noun mag  = u4_ct(u4_ct(daf));

  if ( u4_n_cell(mag) && u4_n_eq(fos, u4_ch(mag)) ) {
    return u4_k_cell(lane, _mill_dump(m, bir), _dump_all(m, mag));
  }
  else return u4_k_trel
    (lane, _mill_dump(m, bir), _mill_dump(m, mag), u4_noun_0);
}

/* _dump_heel(): normalize type.
*/
static u4_type
_dump_heel(u4_milr m,
           u4_type muf)
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
    return u4_k_trel(lane, u4_atom_bone, p_muf, _dump_heel(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_cone, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_cone, _dump_count(lane, q_muf), _dump_heel(m, p_muf));
  }
  else if ( u4_b_p(muf, u4_atom_cube, &p_muf) ) {
    return muf;
  }
  else if ( u4_b_pq(muf, u4_atom_fork, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_fork, _dump_heel(m, p_muf), _dump_heel(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_fuse, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_fuse, _dump_heel(m, p_muf), _dump_heel(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_pair, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_pair, _dump_heel(m, p_muf), _dump_heel(m, q_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_post, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_post, _dump_weight(lane, q_muf), _dump_heel(m, p_muf));
  }
  else if ( u4_b_pq(muf, u4_atom_skin, &p_muf, &q_muf) ) {
    return u4_k_trel
      (lane, u4_atom_skin, p_muf, _dump_heel(m, q_muf));
  }
  else return u4_trip;
}

/* _mill_dump(): type to prep.
*/
u4_prep
_mill_dump(u4_milr m,
           u4_type daf)
{
  return u4_pump_prep(m->lane, _dump_heel(m, daf));
}
#else

static u4_prep
_mill_dump_in(u4_milr m,
              u4_tab  nip,
              u4_xw   xw_zar,
              u4_type daf)
{
  u4_lane lane = m->lane;
  u4_noun p_daf, q_daf;

  if ( u4_n_atom(daf) ) {
    return daf;
  }

  else if ( u4_b_pq(daf, u4_atom_bone, &p_daf, &q_daf) ) {
    u4_atom gat;

    gat = u4_k_atom_cat
      (lane, u4_atom_bone, 
             u4_k_atom_cat(lane, u4_cod_in('/'), 
                                 u4_prep_decimal(lane, p_daf)));

    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane, gat, _mill_dump_in(m, nip, xw_zar, q_daf), 0));
  }

  else if ( u4_b_pq(daf, u4_atom_cone, &p_daf, &q_daf) ) {
    u4_atom gat;

    gat = u4_k_atom_cat
      (lane, u4_atom_cone, 
             u4_k_atom_cat
               (lane, u4_cod_in('+'), 
                      u4_prep_decimal(lane, _dump_count(lane, q_daf))));

    return u4_prep_close
      (lane, '(', ')', 
       u4_k_list(lane, gat, _mill_dump_in(m, nip, xw_zar, p_daf), 0));
  }

  else if ( u4_b_p(daf, u4_atom_cube, &p_daf) ) {
    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane, u4_atom_cube, u4_pump_prep(lane, p_daf), 0));
  }

  else if ( u4_b_pq(daf, u4_atom_fork, &p_daf, &q_daf) ) {
    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane,
                       u4_atom_fork,
                       _mill_dump_in(m, nip, xw_zar, p_daf),
                       _mill_dump_in(m, nip, xw_zar, q_daf),
                       0));
  }

  else if ( u4_b_pq(daf, u4_atom_fuse, &p_daf, &q_daf) ) {
    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane,
                       u4_atom_fuse,
                       _mill_dump_in(m, nip, xw_zar, p_daf),
                       _mill_dump_in(m, nip, xw_zar, q_daf),
                       0));
  }

  else if ( u4_b_pq(daf, u4_atom_pair, &p_daf, &q_daf) ) {
    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane,
                       u4_atom_pair,
                       _mill_dump_in(m, nip, xw_zar, p_daf),
                       _mill_dump_in(m, nip, xw_zar, q_daf),
                       0));
  }

  else if ( u4_b_pq(daf, u4_atom_post, &p_daf, &q_daf) ) {
#if 1
    if ( u4_tab_in(daf, nip) ) {
      return u4_k_atom_cat
        (lane, u4_cod_in('#'), u4_tab_get(daf, nip));
    }
    else {
      u4_prep vof;

      nip = u4_tab_add(lane, daf, u4_cod_in('A' + xw_zar), nip); 
      vof = _mill_dump_in(m, nip, xw_zar + 1, _mill_reap(m, daf));

      return u4_k_quil
        (lane, u4_atom_nail, 
               u4_cod_in('A' + xw_zar),
               u4_cod_in('#'), 
               vof,
               u4_noun_0);
    }
#else
    u4_atom gat;

    gat = u4_k_atom_cat
      (lane, u4_atom_post, 
             u4_k_atom_cat
               (lane, u4_cod_in(':'), 
                      u4_prep_decimal(lane, _dump_weight(lane, q_daf))));

    return u4_prep_close
      (lane, '(', ')', 
       u4_k_list(lane, gat, _mill_dump_in(m, nip, xw_zar, p_daf), 0));
#endif
  }

  else if ( u4_b_pq(daf, u4_atom_skin, &p_daf, &q_daf) ) {
    u4_atom gat;

    gat = u4_k_atom_cat
      (lane, u4_atom_skin, u4_k_atom_cat(lane, u4_cod_in('='), p_daf)); 

    return u4_prep_close
      (lane, '(', ')', 
       u4_k_list(lane, gat, _mill_dump_in(m, nip, xw_zar, q_daf), 0));
  }

  else {
    return u4_trip;
  }
}

u4_prep
_mill_dump(u4_milr m,
           u4_type daf)
{
  return _mill_dump_in(m, u4_noun_0, 0, daf);

}
#endif
