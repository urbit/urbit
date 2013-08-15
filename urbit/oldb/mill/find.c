/* mill/find.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

    /** Forward declarations.
    **/
      static u4_loaf
      _find_main(u4_milr, u4_mark, u4_bag, u4_rail, u4_mold);


/* _find_hang(): attach a loaf by an axis.
*/
static u4_loaf
_find_hang(u4_milr m,
           u4_axis axe,
           u4_loaf tub)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(tub) ) {
    return tub;
  }
  else {
    return u4_k_cell
      (lane, 
       u4_ch(tub), 
       _mill_comp(m, u4_k_cell(lane, u4_noun_0, axe), u4_ct(tub)));
  }
}

/* _find_bink(): as _find_main(), but typ is at axis.
*/
static u4_loaf
_find_bink(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_axis axe,
           u4_rail bar,
           u4_mold typ)
{
  return 
    _find_hang(m, axe, 
                  _find_main(m, cox, gil, _mill_slip(m, axe, bar), typ));
}

/* _find_seek(): search in book with axis.  Produces |(~, [axis gene]).
*/
static u4_carb
_find_seek(u4_milr m,
           u4_axis axe,
           u4_mark cox,
           u4_book hig)
{
  u4_lane lane  = m->lane;
  u4_noun p_hig = u4_ch(hig);
  u4_noun q_hig = u4_ct(hig);

  if ( !u4_n_atom(p_hig) ) {
    u4_carb bek = _find_seek(m, u4_op_peg(lane, axe, u4_noun_2), cox, p_hig);
    u4_carb lod = _find_seek(m, u4_op_peg(lane, axe, u4_noun_3), cox, q_hig);

    if ( !u4_n_zero(bek) ) {
      if ( !u4_n_zero(lod) ) {
        return _mill_fail(m, "find: book conflict");
      }
      else return bek;
    }
    else return lod;
  }
  else {
    if ( u4_n_eq(cox, p_hig) ) {
      return u4_k_cell(lane, axe, q_hig);
    }
    else return u4_noun_0;
  }
}

/* _find_cell(): as _find_main(), for [%cell p_typ q_typ].
*/
static u4_loaf
_find_cell(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mold p_typ,
           u4_mold q_typ)
{
  u4_loaf gul = _find_bink(m, cox, gil, u4_noun_2, bar, p_typ);
  u4_loaf das = _find_bink(m, cox, gil, u4_noun_3, bar, q_typ);

  if ( !u4_n_zero(gul) && !u4_n_zero(das) ) {
    return _mill_fail(m, "find: cell conflict");
  }
  else if ( !u4_n_zero(gul) ) {
    return gul;
  }
  else return das;
}

/* _find_mono(): as _find_main(), for [%mono p_typ q_typ].
*/
static u4_loaf
_find_mono(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mold p_typ,
           u4_book q_typ)
{
  u4_lane lane = m->lane;
  u4_carb gos = _find_seek(m, u4_noun_1, cox, q_typ);

  if ( u4_n_zero(gos) ) {
    return _find_bink(m, cox, gil, u4_noun_2, bar, p_typ);
  }
  else {
    u4_axis p_gos = u4_ch(gos);
    u4_gene q_gos = u4_ct(gos);
    u4_axis gal   = u4_op_peg(lane, u4_noun_3, p_gos);

    return u4_k_cell
      (lane,
       u4_k_trel(lane, u4_atom_hold, 
                       u4_k_trel(lane, u4_atom_mono, p_typ, q_typ),
                       q_gos),
       u4_k_trel(lane, u4_noun_3,
                       u4_k_cell(lane, u4_noun_0, u4_noun_1),
                       u4_k_cell(lane, u4_noun_0, gal)));
  }
}

/* _find_poly(): as _find_main(), for [%poly p_typ q_typ].
*/
static u4_loaf
_find_poly(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mold p_typ,
           u4_book q_typ)
{
  u4_lane lane = m->lane;
  u4_carb gos = _find_seek(m, u4_noun_1, cox, q_typ);

  if ( u4_n_zero(gos) ) {
    return _find_bink(m, cox, gil, u4_noun_2, bar, p_typ);
  }
  else {
    u4_axis p_gos = u4_ch(gos);
    u4_gene q_gos = u4_ct(gos);
    u4_axis gal   = u4_op_peg(lane, u4_noun_3, p_gos);

    return u4_k_cell
      (lane,
       u4_k_trel(lane, u4_atom_hold, 
                       u4_k_trel(lane, u4_atom_poly, p_typ, q_typ),
                       q_gos),
       u4_k_trel(lane, u4_noun_3,
                       u4_k_cell(lane, u4_noun_0, u4_noun_1),
                       u4_k_cell(lane, u4_noun_0, gal)));
  }
}

/* _find_face(): as _find_main(), for [%face p_typ q_typ]
*/
static u4_loaf
_find_face(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mark p_typ,
           u4_mold q_typ)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(cox, p_typ) ) {
    return u4_k_trel(lane, q_typ, u4_noun_0, u4_noun_1);
  }
  else return u4_noun_0;
}

/* _find_forq(): as _find_main(), for [%forq p_typ].
*/
static u4_loaf
_find_forq(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_log  p_typ)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_typ) ) {
    return u4_noun_1;
  }
  else {
    u4_mold ip_typ = u4_ch(p_typ);
    u4_log  tp_typ = u4_ct(p_typ);

    if ( _mill_cull(m, bar, ip_typ) ||
          u4_bag_in(u4_k_cell(lane, bar, p_typ), gil) )
    {
      return _find_forq(m, cox, gil, bar, tp_typ);
    }
    else {
      u4_loaf dor = _find_main(m, cox, gil, bar, ip_typ);
      u4_loaf hum = _find_forq(m, cox, gil, bar, tp_typ);

      if ( u4_n_eq(u4_noun_1, hum) ) {
        return dor;
      }
      else {
        if ( u4_n_zero(dor) && u4_n_zero(hum) ) {
          return u4_noun_0;
        }
        else if ( !u4_n_zero(dor) && 
                  !u4_n_zero(hum) &&
                  u4_n_eq(u4_ct(dor), u4_ct(hum)) )
        {
          return u4_k_cell
            (lane, _mill_eith(m, u4_ch(dor), u4_ch(hum)),
                   u4_ct(dor));
        }
        else {
          return _mill_fail(m, "find: forq conflict");
        }
      }
    }
  }
} 

/* _find_fuse(): as _find_main(), for [%fuse p_typ q_typ].
*/
static u4_loaf
_find_fuse(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mold p_typ,
           u4_mold q_typ)
{
  u4_lane lane = m->lane;
  u4_rail dax  = u4_k_cell(lane, p_typ, bar);
  u4_loaf pir  = _find_main(m, cox, gil, dax, q_typ);

  if ( u4_n_zero(pir) ) {
    u4_loaf rex = _find_main(m, cox, gil, bar, p_typ);

    if ( u4_n_zero(rex) ) {
      return u4_noun_0;
    } else {
      u4_axis axe;

      if ( u4_b_p(u4_ct(rex), u4_noun_0, &axe) ) {
        return u4_k_cell
          (lane, _mill_both(m, u4_ch(rex), _mill_peek(m, axe, dax, q_typ)),
                 u4_ct(rex)); 
      }
      else return rex;
    }
  }
  else {
    u4_mold gan = u4_ch(pir);
    u4_nock fol = u4_ct(pir);
    u4_axis axe;
    u4_mold buc;

    if ( u4_b_p(fol, u4_noun_0, &axe) ) {
      u4_loaf rex = _find_main(m, cox, gil, bar, p_typ);

      if ( u4_n_zero(rex) ) {
        buc = _mill_both(m, _mill_peek(m, axe, bar, p_typ), gan);
      }
      else if ( u4_n_eq(fol, u4_ct(rex)) ) {
        buc = _mill_both(m, u4_ch(rex), gan);
      } 
      else return _mill_fail(m, "find: fuse conflict");
    }
    else buc = gan;

#if 0
    else if ( u4_b_pq(fol, u4_noun_3, &p_fol, 0) &&
              u4_b_p(p_fol, u4_noun_0, &axe) &&
              u4_b_pq(gan, u4_atom_hold, &p_gan, &q_gan) ) 
    {
      // Fusion polymorphism.  Very powerful and dangerous.
      //
      buc = u4_k_trel
        (lane, u4_atom_hold, 
               _mill_both(m, _mill_peek(m, axe, bar, p_typ), p_gan),
               q_gan);
    }
    else return u4_trip;
#endif

    return u4_k_cell(lane, buc, fol);
  }
}

/* _find_main(): as _find_main(), with gil.
*/
static u4_loaf
_find_main(u4_milr m,
           u4_mark cox,
           u4_bag  gil,
           u4_rail bar,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_noun p_typ, q_typ;

  // %atom
  // %blur
  //
  if ( u4_n_eq(u4_atom_atom, typ) ||
       u4_n_eq(u4_atom_blot, typ) ||
       u4_n_eq(u4_atom_blur, typ) )
  {
    return u4_noun_0;
  }

  // [%cell p=mold q=mold]
  //
  else if ( u4_b_pq(typ, u4_atom_cell, &p_typ, &q_typ) ) {
    return _find_cell(m, cox, gil, bar, p_typ, q_typ);
  }

  // [%mono p=mold q=bush+[mark mold]]
  //
  else if ( u4_b_pq(typ, u4_atom_mono, &p_typ, &q_typ) ) {
    return _find_mono(m, cox, gil, bar, p_typ, q_typ);
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(typ, u4_atom_cube, &p_typ) ) {
    return u4_noun_0;
  }

  // [%poly p=mold q=bush+[mark mold]]
  //
  else if ( u4_b_pq(typ, u4_atom_poly, &p_typ, &q_typ) ) {
    return _find_poly(m, cox, gil, bar, p_typ, q_typ);
  }

  // [%face p=mark q=mold]
  //
  else if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
    return _find_face(m, cox, gil, bar, p_typ, q_typ);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(typ, u4_atom_forq, &p_typ) ) {
    u4_loaf dox = _find_forq(m, cox, gil, bar, p_typ);

    if ( u4_n_eq(dox, u4_noun_1) ) {
      return u4_noun_0;
    } else return dox;
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
    return _find_fuse(m, cox, gil, bar, p_typ, q_typ);
  }

  // [%hold p=mold q=gene]
  //
  else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
    u4_noun dit = u4_k_cell(lane, bar, typ);

    if ( u4_bag_in(dit, gil) ) {
      return u4_noun_0;
    } else {
      return _find_main
        (m, cox,
            u4_bag_add(lane, dit, gil), 
            bar,
            _mill_repo(m, p_typ, q_typ));
    }
  }

  else {
    u4_bug("strange mold", typ);
    return u4_trip;
  }
}

/* _mill_find(): look by name.
*/
u4_loaf
_mill_find(u4_milr m,
           u4_mark cox,
           u4_rail bar,
           u4_mold typ)
{
  u4_noun tog = u4_k_trel(m->lane, cox, bar, typ);
  u4_nopt zax;

  zax = u4_tab_get(tog, m->fin); 

  if ( u4_bull != zax ) {
    return zax;
  } 
  else {
    zax = _find_main(m, cox, u4_noun_0, bar, typ);

    m->fin = u4_tab_add(m->lane, tog, zax, m->fin);
    return zax;
  }
}
