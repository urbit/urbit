/* mill/nest.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_t _nest_main(u4_milr, u4_bag, u4_rail, u4_mold, u4_mold);


/* _nest_flat(): true iff [meg nax] is flat.
*/
static u4_t
_nest_flat(u4_milr m,
           u4_bag  hax,
           u4_rail meg,
           u4_mold nax)
{
  u4_lane lane = m->lane;
  u4_noun p_nax, q_nax;

  // %atom
  //
  if ( u4_n_eq(u4_atom_atom, nax) ) {
    return u4_true;
  }

  // %blot
  //
  else if ( u4_n_eq(u4_atom_blot, nax) ) {
    return u4_true;
  }

  // %blur
  //
  else if ( u4_n_eq(u4_atom_blur, nax) ) {
    return u4_false;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_cell, &p_nax, &q_nax) ) {
    return u4_false;
  }

  // [%mono p=mold q=bush+[term mold]]
  // [%poly p=mold q=bush+[term mold]]
  //
  else if ( u4_b_pq(nax, u4_atom_mono, &p_nax, &q_nax) ||
            u4_b_pq(nax, u4_atom_poly, &p_nax, &q_nax) ) {
    return u4_false;
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(nax, u4_atom_cube, &p_nax) ) {
    if ( u4_n_atom(p_nax) ) {
      return u4_true;
    }
    else return u4_false;
  }

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(nax, u4_atom_face, &p_nax, &q_nax) ) {
    return _nest_flat(m, hax, meg, q_nax);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(nax, u4_atom_forq, &p_nax) ) {
    while ( 1 ) {
      if ( u4_n_zero(p_nax) ) {
        return u4_true;
      }
      else {
        u4_mold ip_nax = u4_ch(p_nax);
        u4_log  tp_nax = u4_ct(p_nax);

        if ( !_mill_cull(m, meg, ip_nax) &&
             !_nest_flat(m, hax, meg, ip_nax) ) 
        {
          return u4_false;
        }
        else p_nax = tp_nax;
      }
    }
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_fuse, &p_nax, &q_nax) ) {
    return _nest_flat(m, hax, meg, p_nax) ||
           _nest_flat(m, hax, u4_k_cell(lane, p_nax, meg), q_nax);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(nax, u4_atom_hold, &p_nax, &q_nax) ) {
    u4_noun dit = u4_k_cell(lane, meg, nax);

    if ( u4_bag_in(dit, hax) ) {
      return 1;
    } else {
      return _nest_flat(m, u4_bag_add(lane, dit, hax), 
                           meg, 
                           _mill_repo(m, p_nax, q_nax));
    }
  }
  else return u4_trip;
}

/* _nest_deep(): true iff [meg nax] is deep (known non-atomic).
*/
static u4_t
_nest_deep(u4_milr m,
           u4_bag  hax,
           u4_rail meg,
           u4_mold nax)
{
  u4_lane lane = m->lane;
  u4_noun p_nax, q_nax;

  // %atom
  //
  if ( u4_n_eq(u4_atom_atom, nax) ) {
    return u4_false;
  }

  // %blot
  //
  else if ( u4_n_eq(u4_atom_blot, nax) ) {
    return u4_true;
  }

  // %blur
  //
  else if ( u4_n_eq(u4_atom_blur, nax) ) {
    return u4_false;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_cell, &p_nax, &q_nax) ) {
    return u4_true;
  }

  // [%mono p=mold q=bush+[term mold]]
  // [%poly p=mold q=bush+[term mold]]
  //
  else if ( u4_b_pq(nax, u4_atom_mono, &p_nax, &q_nax) ||
            u4_b_pq(nax, u4_atom_poly, &p_nax, &q_nax) ) {
    return u4_true;
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(nax, u4_atom_cube, &p_nax) ) {
    if ( u4_n_atom(p_nax) ) {
      return u4_false;
    }
    else return u4_true;
  }

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(nax, u4_atom_face, &p_nax, &q_nax) ) {
    return _nest_deep(m, hax, meg, q_nax);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(nax, u4_atom_forq, &p_nax) ) {
    while ( 1 ) {
      if ( u4_n_zero(p_nax) ) {
        return u4_true;
      }
      else {
        u4_mold ip_nax = u4_ch(p_nax);
        u4_log  tp_nax = u4_ct(p_nax);

        if ( !_mill_cull(m, meg, ip_nax) &&
             !_nest_deep(m, hax, meg, ip_nax) ) 
        {
          return u4_false;
        }
        else p_nax = tp_nax;
      }
    }
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_fuse, &p_nax, &q_nax) ) {
    return _nest_deep(m, hax, meg, p_nax) ||
           _nest_deep(m, hax, u4_k_cell(lane, p_nax, meg), q_nax);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(nax, u4_atom_hold, &p_nax, &q_nax) ) {
    u4_noun dit = u4_k_cell(lane, meg, nax);

    if ( u4_bag_in(dit, hax) ) {
      return 1;
    } else {
      return _nest_deep(m, u4_bag_add(lane, dit, hax), 
                           meg, 
                           _mill_repo(m, p_nax, q_nax));
    }
  }
  else return u4_trip;
}

/* _nest_rock(): congruence for atomic constant.
*/
static u4_t
_nest_rock(u4_milr m,
           u4_bag  hax,
           u4_noun p_typ,
           u4_rail meg,
           u4_mold nax)
{
  u4_lane lane = m->lane;
  u4_noun p_nax, q_nax;

  // %atom
  //
  if ( u4_n_eq(u4_atom_atom, nax) ) {
    return u4_false;
  }

  // %blot
  //
  else if ( u4_n_eq(u4_atom_blot, nax) ) {
    return u4_true;
  }

  // %blur
  //
  else if ( u4_n_eq(u4_atom_blur, nax) ) {
    return u4_false;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_cell, &p_nax, &q_nax) ) {
    return u4_false;
  }

  // [%mono p=mold q=bush+[term mold]]
  // [%poly p=mold q=bush+[term mold]]
  //
  else if ( u4_b_pq(nax, u4_atom_mono, &p_nax, &q_nax) ||
            u4_b_pq(nax, u4_atom_poly, &p_nax, &q_nax) ) {
    return u4_false;
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(nax, u4_atom_cube, &p_nax) ) {
    return u4_n_eq(p_typ, p_nax);
  }

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(nax, u4_atom_face, &p_nax, &q_nax) ) {
    return _nest_rock(m, hax, p_typ, meg, q_nax);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(nax, u4_atom_forq, &p_nax) ) {
    while ( 1 ) {
      if ( u4_n_zero(p_nax) ) {
        return u4_true;
      }
      else {
        u4_mold ip_nax = u4_ch(p_nax);
        u4_log  tp_nax = u4_ct(p_nax);

        if ( !_mill_cull(m, meg, ip_nax) &&
             !_nest_rock(m, hax, p_typ, meg, ip_nax) ) 
        {
          return u4_false;
        }
        else p_nax = tp_nax;
      }
    }
  }


  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_fuse, &p_nax, &q_nax) ) {
    return _nest_rock(m, hax, p_typ, meg, p_nax) ||
           _nest_rock(m, hax, p_typ, u4_k_cell(lane, p_nax, meg), q_nax);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(nax, u4_atom_hold, &p_nax, &q_nax) ) {
    u4_noun dit = u4_k_cell(lane, meg, nax);

    if ( u4_bag_in(dit, hax) ) {
      return 1;
    } else {
      return _nest_rock(m, u4_bag_add(lane, dit, hax), 
                           p_typ,
                           meg, 
                           _mill_repo(m, p_nax, q_nax));
    }
  }
  else return u4_trip;
}

/* _nest_cell(): congruence for cell.
*/
static u4_t
_nest_cell(u4_milr m,
           u4_bag  gil,
           u4_mold p_typ,
           u4_mold q_typ,
           u4_rail meg,
           u4_mold nax)
{
  if ( !_nest_deep(m, u4_noun_0, meg, nax) ) {
    return u4_false;
  }
  else {
    u4_noun p_nax = _mill_peek(m, u4_noun_2, meg, nax);
    u4_noun q_nax = _mill_peek(m, u4_noun_3, meg, nax);
    u4_rail p_meg = _mill_slip(m, u4_noun_2, meg);
    u4_rail q_meg = _mill_slip(m, u4_noun_3, meg);

    return _nest_main(m, gil, p_meg, p_nax, p_typ) &&
           _nest_main(m, gil, q_meg, q_nax, q_typ);
  }
}

/* _nest_mono(): congruence for mono or poly.
*/
static u4_t
_nest_mono(u4_milr m,
           u4_bag  gil,
           u4_bag  hax,
           u4_mold p_typ,
           u4_noun q_typ,
           u4_rail meg,
           u4_mold nax)
{
  u4_lane lane = m->lane;
  u4_noun p_nax, q_nax;

  // %atom
  //
  if ( u4_n_eq(u4_atom_atom, nax) ) {
    return u4_false;
  }

  // %blot
  //
  else if ( u4_n_eq(u4_atom_blot, nax) ) {
    return u4_true;
  }

  // %blur
  //
  else if ( u4_n_eq(u4_atom_blur, nax) ) {
    return u4_false;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_cell, &p_nax, &q_nax) ) {
    return u4_false;
  }

  // [%mono p=mold q=bush+[term mold]]
  // [%poly p=mold q=bush+[term mold]]
  //
  else if ( u4_b_pq(nax, u4_atom_mono, &p_nax, &q_nax)  ||
            u4_b_pq(nax, u4_atom_poly, &p_nax, &q_nax) )
  {
    return _nest_main(m, gil, _mill_slip(m, u4_noun_2, meg), p_nax, p_typ) &&
           u4_n_eq(q_typ, q_nax);
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(nax, u4_atom_cube, &p_nax) ) {
    return u4_false;
  }

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(nax, u4_atom_face, &p_nax, &q_nax) ) {
    return _nest_mono(m, gil, hax, p_typ, q_typ, meg, q_nax);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(nax, u4_atom_forq, &p_nax) ) {
    while ( 1 ) {
      if ( u4_n_zero(p_nax) ) {
        return u4_true;
      }
      else {
        u4_mold ip_nax = u4_ch(p_nax);
        u4_log  tp_nax = u4_ct(p_nax);

        if ( !_mill_cull(m, meg, ip_nax) &&
             !_nest_mono(m, gil, hax, p_typ, q_typ, meg, ip_nax) ) 
        {
          return u4_false;
        }
        else p_nax = tp_nax;
      }
    }
  }


  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(nax, u4_atom_fuse, &p_nax, &q_nax) ) {
    return _nest_mono(m, gil, hax, p_typ, q_typ, meg, p_nax) ||
           _nest_mono(m, gil, hax, p_typ, 
                                   q_typ, 
                                   u4_k_cell(lane, p_nax, meg), 
                                   q_nax);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(nax, u4_atom_hold, &p_nax, &q_nax) ) {
    u4_noun dit = u4_k_cell(lane, meg, nax);

    if ( u4_bag_in(dit, hax) ) {
      return 1;
    } else {
      return _nest_mono(m, gil, u4_bag_add(lane, dit, hax), 
                                p_typ,
                                q_typ, 
                                meg, 
                                _mill_repo(m, p_nax, q_nax));
    }
  }
  else return u4_trip;
}

/* _nest_forq_row()::
*/
static u4_t
_nest_forq_row(u4_milr m,
               u4_bag  gil,
               u4_log  p_typ,
               u4_rail meg,
               u4_mold nax)
{
  if ( u4_n_zero(p_typ) ) {
    return u4_false;
  }
  else {
    return _nest_main(m, gil, meg, nax, u4_ch(p_typ)) ||
           _nest_forq_row(m, gil, u4_ct(p_typ), meg, nax);
  }
}

/* _nest_forq_col()::
*/
static u4_t
_nest_forq_col(u4_milr m,
               u4_bag  gil,
               u4_log  p_typ,
               u4_rail meg,
               u4_log  p_nax)
{
  if ( u4_n_zero(p_nax) ) {
    return u4_true;
  }
  else {
    u4_mold ip_nax = u4_ch(p_nax);
    u4_mold tp_nax = u4_ct(p_nax);

    if ( _mill_cull(m, meg, ip_nax) ) {
      return _nest_forq_col(m, gil, p_typ, meg, tp_nax);
    }
    else {
      return _nest_forq_row(m, gil, p_typ, meg, ip_nax) &&
             _nest_forq_col(m, gil, p_typ, meg, tp_nax);
    }
  }
}

/* _nest_forq(): nest a forq.
*/
static u4_t
_nest_forq(u4_milr m,
           u4_bag  gil,
           u4_bag  hax,
           u4_log  p_typ,
           u4_rail meg,
           u4_mold nax)
{
  u4_lane lane = m->lane;
  u4_noun p_nax, q_nax;

  if ( u4_b_pq(nax, u4_atom_face, &p_nax, &q_nax) ) {
    return _nest_forq(m, gil, hax, p_typ, meg, q_nax);
  }
  else if ( u4_b_pq(nax, u4_atom_hold, &p_nax, &q_nax) ) {
    u4_noun dit = u4_k_cell(lane, meg, nax);

    if ( u4_bag_in(dit, hax) ) {
      return 1;
    } else {
      return _nest_forq(m, gil, 
                           u4_bag_add(lane, dit, hax), 
                           p_typ,
                           meg, 
                           _mill_repo(m, p_nax, q_nax));
    }
  }
  else if ( u4_b_p(nax, u4_atom_forq, &p_nax) ) {
    return _nest_forq_col(m, gil, p_typ, meg, p_nax);
  }
  else {
    return _nest_forq_row(m, gil, p_typ, meg, nax);
  }
}

/* _nest_loop(): innards of _nest_main().
*/
static u4_t
_nest_loop(u4_milr m,
           u4_bag  gil,
           u4_rail meg,
           u4_mold nax,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_noun p_typ, q_typ;

  m->prf++;

  // %atom
  // 
  if ( u4_n_eq(u4_atom_atom, typ) ) {
    return _nest_flat(m, u4_noun_0, meg, nax);
  }

  // %blot
  //
  else if ( u4_n_eq(u4_atom_blot, typ) ) {
    return 0;
  }

  // %blur
  //
  else if ( u4_n_eq(u4_atom_blur, typ) ) {
    return 1;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(typ, u4_atom_cell, &p_typ, &q_typ) ) {
    return _nest_cell(m, gil, p_typ, q_typ, meg, nax);
  }

  // [%mono p=mold q=bush+[term mold]]
  // [%poly p=mold q=bush+[term mold]]
  //
  else if ( u4_b_pq(typ, u4_atom_mono, &p_typ, &q_typ) || 
            u4_b_pq(typ, u4_atom_poly, &p_typ, &q_typ) ) {
    return _nest_mono(m, gil, u4_noun_0, p_typ, q_typ, meg, nax);
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(typ, u4_atom_cube, &p_typ) ) {
    if ( u4_n_cell(p_typ) ) {
      return _nest_main(m, gil, meg, nax, _mill_reap(m, typ));
    }
    else {
      return _nest_rock(m, u4_noun_0, p_typ, meg, nax);
    }
  }

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
    return _nest_main(m, gil, meg, nax, q_typ);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_pq(typ, u4_atom_forq, 0, 0) ) {
    return _nest_forq(m, gil, u4_noun_0, p_typ, meg, nax);
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
    return _nest_main(m, gil, meg, nax, p_typ) &&
           _nest_main(m, gil, meg, nax, q_typ);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
    return _nest_main(m, gil, meg, nax, _mill_repo(m, p_typ, q_typ));
  }

  else {
    u4_burp(lane, "typ", _mill_dump(m, typ));
    return u4_trip;
  }
}

/* _nest_main(): geometric congruence, with loop control (gil).
*/
static u4_t
_nest_main(u4_milr m,
           u4_bag  gil,
           u4_rail meg,
           u4_mold nax,
           u4_mold typ)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(nax, typ) ) {
    return 1;
  }
  else {
    u4_noun res = u4_k_trel(lane, meg, nax, typ);

    if ( u4_bag_in(res, gil) ) {
      // Conservative search.
      //
      return 1;
    }
    else {
      u4_nopt zod = u4_tab_get(res, m->vus);

      if ( zod != u4_bull ) {
        return u4_n_zero(zod);
      }
      else {
        u4_t t_zod = _nest_loop(m, u4_bag_add(lane, res, gil), meg, nax, typ);

        zod = (t_zod ? u4_noun_0 : u4_noun_1);

        m->vus = u4_tab_add(m->lane, res, zod, m->vus);
        return t_zod;
      }
    }
  }
}

/* _mill_nest(): test geometric congruence.
**
**    [nax] is geometrically congruent with [typ] iff 
**    every noun in [nax] is also in [typ].  Ie: [nax] <: mold.
*/
u4_t
_mill_nest(u4_milr m,
           u4_mold nax,
           u4_mold typ)
{
  u4_t nes;
  int  pr0=m->prf;

  nes = _mill_null(m, nax) || _nest_main(m, u4_noun_0, u4_noun_0, nax, typ);

  if ( !u4_n_zero(m->rux) && ((m->prf - pr0) >= 10000) ) {
    printf("\nhuge %d\n", m->prf - pr0);
    u4_burp(m->lane, "nax", _mill_dump(m, nax));
    u4_burp(m->lane, "typ", _mill_dump(m, typ));
    abort();
  }
  return nes;
}
