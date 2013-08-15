/* mill/dust.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  static u4_type _mill_dust_main(u4_milr m, u4_noun lor, u4_type bex);

#if 0
  static u4_prep
  _prep_dust_in(u4_milr m,
                u4_noun lor)
  {
    if ( u4_n_zero(lor) ) {
      return u4_noun_0;
    }
    else {
      return 
        u4_k_cell
          (m->lane, 
           u4_k_qual(m->lane, 
                     u4_atom_glue,
                     u4_prep_decimal(m->lane, u4_chh(lor)),
                     _mill_dump(m, u4_cht(lor)),
                     u4_noun_0),
           _prep_dust_in(m, u4_ct(lor)));
    }
  }

  static u4_prep
  _prep_dust(u4_milr m,
             u4_noun lor)
  {
    return u4_k_cell(m->lane, u4_atom_glue, _prep_dust_in(m, lor));
  }
#endif
  
/* _dust_bump(): replace whole type.
*/
static u4_type
_dust_bump(u4_milr m,
           u4_type faw,
           u4_type bex)
{
  u4_lane lane = m->lane;
  u4_noun p_bex, q_bex;

  if ( u4_b_pq(bex, u4_atom_fuse, &p_bex, &q_bex) ) {
    return _dust_bump(m, faw, q_bex);
  }
  else if ( u4_b_pq(bex, u4_atom_pair, &p_bex, &q_bex) ) {
    u4_noun p_faw, q_faw;

    if ( u4_b_pq(faw, u4_atom_pair, &p_faw, &q_faw) ) {
      return u4_k_trel
        (lane, u4_atom_pair,
               _dust_bump(m, p_faw, p_bex),
               _dust_bump(m, q_faw, q_bex));
    }
    else return faw;
  }
  else if ( u4_b_pq(bex, u4_atom_skin, &p_bex, &q_bex) ) {
    return u4_k_trel(lane, u4_atom_skin, p_bex, _dust_bump(m, faw, q_bex));
  }

  else return faw;
}

/* _dust_cone_book()::
*/
static u4_t 
_dust_cone_book(u4_milr m,
                u4_type nar,
                u4_type bex,
                u4_noun q_bex)
{
  if ( u4_n_atom(u4_ch(q_bex)) ) {
    u4_form duf = _mill_make(m, nar, u4_ct(q_bex));
    u4_form nac = _mill_make(m, bex, u4_ct(q_bex));

    return u4_n_eq(nac, duf);
  }
  else {
    return _dust_cone_book(m, nar, bex, u4_ch(q_bex)) && 
           _dust_cone_book(m, nar, bex, u4_ct(q_bex));
  }
}

/* _dust_cone()::
*/
static u4_type
_dust_cone(u4_milr m,
           u4_type gam,
           u4_type nog,
           u4_type p_bex,
           u4_noun q_bex)
{
  if ( !u4_n_zero(nog) ) {
    return _mill_fail(m, "cone burn");
  }
  else {
    u4_type fov = _mill_dust_main(m, gam, p_bex);

    /* We don't replace synonyms.  This rule prevents at least
    ** one class of infinite loop.
    */
    if ( _mill_cong(m, p_bex, fov) && _mill_cong(m, fov, p_bex) ) {
      return u4_k_trel(m->lane, u4_atom_cone, p_bex, q_bex);
    }
    else return u4_k_trel(m->lane, u4_atom_cone, fov, q_bex);
  }
}

/* _dust_bone():: dust a bone.
*/
static u4_type
_dust_bone(u4_milr m,
           u4_log  gam,
           u4_log  nog,
           u4_axis p_bex,
           u4_type q_bex)
{
  u4_lane lane = m->lane;
  u4_axis vot  = u4_op_tip(p_bex);
  u4_axis myr  = u4_op_tap(lane, p_bex);
  u4_type leq  = _mill_flap(m, myr, q_bex);
  u4_log  nel  = (u4_n_eq(u4_noun_2, vot) ? gam : nog);
  u4_type paz = _mill_dust_main(m, nel, leq);

  return _mill_flap(m, vot, paz);
}

/* _dust_fall():: partial dust.
*/
static u4_type
_dust_fall(u4_milr m,
           u4_log  gam,
           u4_log  nog,
           u4_type bex)
{
  u4_lane lane = m->lane;
  u4_noun p_bex, q_bex;

  if ( u4_n_eq(u4_atom_atom, bex) ||
       u4_n_eq(u4_atom_blot, bex) ||
       u4_n_eq(u4_atom_blur, bex) ||
       u4_b_p(bex, u4_atom_cube, &p_bex) )
  {
    u4_type cas = _mill_hack(m, u4_noun_2, bex);
    u4_type luf = _mill_hack(m, u4_noun_3, bex);

    return u4_k_trel
      (lane, u4_atom_pair,
             _mill_dust_main(m, gam, cas),
             _mill_dust_main(m, nog, luf));
  }
  else if ( u4_b_pq(bex, u4_atom_bone, &p_bex, &q_bex) ) {
    return _dust_bone(m, gam, nog, p_bex, q_bex);
  }
  else if ( u4_b_pq(bex, u4_atom_cone, &p_bex, &q_bex) ) {
    return _dust_cone(m, gam, nog, p_bex, q_bex);
  }
  else if ( u4_b_pq(bex, u4_atom_fork, &p_bex, &q_bex) ) {
    return _mill_eith
        (m, _dust_fall(m, gam, nog, p_bex),
            _dust_fall(m, gam, nog, q_bex));
  }
  else if ( u4_b_pq(bex, u4_atom_fuse, &p_bex, &q_bex) ) {
    return _mill_both
        (m, _dust_fall(m, gam, nog, p_bex),
            _dust_fall(m, gam, nog, q_bex)
        );
  }
  else if ( u4_b_pq(bex, u4_atom_pair, &p_bex, &q_bex) ) {
    return u4_k_trel
      (lane, u4_atom_pair,
             _mill_dust_main(m, gam, p_bex),
             _mill_dust_main(m, nog, q_bex));
  }
  else if ( u4_b_pq(bex, u4_atom_skin, &p_bex, &q_bex) ) {
    return u4_k_trel
      (lane, u4_atom_skin, p_bex, _dust_fall(m, gam, nog, q_bex));
  }
  else return _dust_fall(m, gam, nog, _mill_reap(m, bex));
}

/* _mill_dust_main(): mutate a type.
**
** lor: (list [axis type])
** bex: type
**
** We know: no axis in lor covers any other.
*/
static u4_type
_mill_dust_main(u4_milr m,
                u4_noun lor,
                u4_type bex)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(lor) ) {
    return bex;
  }
  else {
    u4_noun zif = u4_noun_0;  // zif: *type: hits at axis 1
    u4_noun gam = u4_noun_0;  // gam: *(axis type): hits to the left
    u4_noun nog = u4_noun_0;  // nog: *(axis type): hits to the right

    _mill_trig(lane, lor, &zif, &gam, &nog);

    if ( !u4_n_zero(zif) ) {
      if ( !u4_n_zero(u4_ct(zif)) || !u4_n_zero(gam) || !u4_n_zero(nog) ) {
        return _mill_fail(m, "dust interference");
      }
      else return _dust_bump(m, u4_ch(zif), bex);
    }
    else return _dust_fall(m, gam, nog, bex);
  }
}

/* _mill_dust(): mutate a type.
**
** lor: (list [axis type])
** bex: type
**
** We know: no axis in lor covers any other.
*/
u4_type
_mill_dust(u4_milr m,
           u4_noun lor,
           u4_type bex)
{
  u4_type gaw;

  gaw = _mill_dust_main(m, lor, bex);

#if 0
  if ( !u4_n_zero(m->rux ) ) {
    u4_burp(m->lane, "dust: lor", _prep_dust(m, lor));
    u4_burp(m->lane, "dust: bex", _mill_dump(m, bex));
    u4_burp(m->lane, "dust: gaw", _mill_dump(m, gaw));
    printf("\n");
  }
#endif

  return gaw;
}
