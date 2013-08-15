/* mill/edit.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
      u4_mold
      _edit_mask(u4_milr, u4_plox);


/* _edit_plox_take_half(): produce head or tail of plox.
*/
static u4_plox
_edit_plox_take_half(u4_milr m,
                     u4_axis haf,
                     u4_plox zel)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(zel) ) {
    return u4_noun_0;
  }
  else {
    u4_glep i_zel  = u4_ch(zel);
    u4_plox t_zel  = u4_ct(zel);
    u4_tape pi_zel = u4_ch(i_zel);
    u4_mold qi_zel = u4_ct(i_zel);

    u4_assert(!u4_n_zero(pi_zel));
    {
      u4_tack ipi_zel = u4_ch(pi_zel);
      u4_tape tpi_zel = u4_ct(pi_zel);

      if ( u4_n_eq(u4_atom_axis, u4_ch(ipi_zel)) ) {
        u4_axis axe = u4_ct(ipi_zel);
        
        if ( u4_n_eq(haf, u4_op_tip(axe)) ) {
          u4_axis nem = u4_op_tap(lane, axe);

          if ( !u4_n_eq(u4_noun_1, nem) ) {
            pi_zel = u4_k_cell
              (lane, u4_k_cell(lane, u4_atom_axis, nem), tpi_zel);
          }
          else {
            pi_zel = tpi_zel;
          }
          return u4_k_cell
            (lane, u4_k_cell(lane, pi_zel, qi_zel),
                   _edit_plox_take_half(m, haf, t_zel));
        }
        else {
          return _edit_plox_take_half(m, haf, t_zel);
        }
      }
      else {
        // Cutting through a term.  Probably never executed.  But...
        //
        return _edit_plox_take_half
          (m, haf, u4_k_cell(lane, u4_k_cell(lane, tpi_zel, qi_zel), t_zel));
      }
    }
  }
}

/* _edit_plox_take_hed(): produce head of plox.
*/
static u4_plox
_edit_plox_take_hed(u4_milr m,
                    u4_plox zel)
{
  return _edit_plox_take_half(m, u4_noun_2, zel);
}

/* _edit_plox_take_tal(): produce tail of plox.
*/
static u4_plox
_edit_plox_take_tal(u4_milr m,
                    u4_plox zel)
{
  return _edit_plox_take_half(m, u4_noun_3, zel);
}

/* _edit_plox_kil(): convert all molds in plox to blur.
*/
static u4_plox
_edit_plox_kil(u4_milr m,
               u4_plox zel)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(zel) ) {
    return u4_noun_0;
  }
  else {
    u4_glep i_zel  = u4_ch(zel);
    u4_plox t_zel  = u4_ct(zel);
    u4_tape pi_zel = u4_ch(i_zel);

    return u4_k_cell
      (lane, u4_k_cell(lane, pi_zel, u4_atom_blur),
             _edit_plox_kil(m, t_zel));
  }
}

/* _edit_plox_is_end(): true iff plox is finished.
*/
static u4_t
_edit_plox_is_end(u4_milr m,
                  u4_plox zel,
                  u4_mold *nuf)
{
  if ( u4_n_zero(u4_ct(zel)) && u4_n_zero(u4_chh(zel)) ) {
    *nuf = u4_cht(zel);
    return 1;
  }
  else {
    return 0;
  }
}

/* _edit_plox_take_tag(): see _edit_plox_is_tag().
*/
static u4_plox
_edit_plox_take_tag(u4_milr m,
                    u4_plox zel,
                    u4_mark *cox)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(zel) ) {
    return u4_noun_0;
  }
  else {
    u4_glep i_zel  = u4_ch(zel);
    u4_plox t_zel  = u4_ct(zel);
    u4_tape pi_zel = u4_ch(i_zel);
    u4_mold qi_zel = u4_ct(i_zel);

    u4_assert(!u4_n_zero(pi_zel));
    {
      u4_tack ipi_zel = u4_ch(pi_zel);
      u4_tape tpi_zel = u4_ct(pi_zel);

      if ( u4_n_eq(u4_atom_term, u4_ch(ipi_zel)) ) {
        if ( u4_n_zero(*cox) ) {
          *cox = u4_ct(ipi_zel);
        }
        else u4_assert(u4_n_eq(*cox, u4_ct(ipi_zel)));

        return u4_k_cell
          (lane, u4_k_cell(lane, tpi_zel, qi_zel),
                 _edit_plox_take_tag(m, t_zel, cox));
      }
      else return u4_k_cell
        (lane, i_zel,
               _edit_plox_take_tag(m, t_zel, cox));
    }
  }
}

/* _edit_plox_is_tag(): true iff plox affixes a tag.
*/
static u4_t
_edit_plox_is_tag(u4_milr m,
                  u4_plox zel,
                  u4_mark *cox,
                  u4_plox *hom)
{
  *cox = u4_noun_0;
  *hom = _edit_plox_take_tag(m, zel, cox);

  if ( u4_n_zero(*cox) ) {
    return 0;
  }
  else return 1;
}

/* _edit_fuse(): edit a fuse.
*/
static u4_mold
_edit_fuse(u4_milr m,
           u4_plox zel,
           u4_rail bar,
           u4_mold p_gom,
           u4_mold q_gom)
{
  u4_lane lane = m->lane;
  u4_mold nuf;

  if ( _edit_plox_is_end(m, zel, &nuf) ) {
    return nuf;
  }
  else {
    u4_rail sor  = u4_k_cell(lane, p_gom, bar);
    u4_plox zig  = _edit_plox_kil(m, zel);
    u4_mold vep;
    u4_mold gor;

    vep = _mill_edit(m, zig, bar, p_gom);
    gor = _mill_edit(m, zel, sor, q_gom);

    if ( _mill_nest(m, gor, vep) ) {
      return gor;
    }
    else {
      return u4_k_trel(lane, u4_atom_fuse, vep, gor);
    }
  }
}

/* _edit_poly_fab_gene(): 
*/
static u4_t
_edit_poly_fab_gene(u4_milr m,
                    u4_mold doz,
                    u4_mold p_gom,
                    u4_gene gen)
{
  u4_lane lane = m->lane;
  u4_noun sez  = u4_k_trel(lane, doz, p_gom, gen);

  if ( u4_bag_in(sez, m->pox) ) {
    return 1;
  }
  else {
    u4_bag pox = m->pox;
    u4_t   fab;

    m->pox = u4_bag_add(lane, sez, m->pox);
    {
      u4_nock goc  = _mill_bake(m, gen, doz);
      u4_nock zid  = _mill_bake(m, gen, p_gom);

      fab = u4_n_eq(goc, zid);
    }
    m->pox = pox;

    if ( !fab ) {
      // printf("\n");
      // u4_burp(lane, "edit: doz", _mill_dump(m, doz));
      // u4_burp(lane, "edit: pib", _mill_dump(m, p_gom));
      // u4_err(lane, "edit: gen", gen);
    }
    return fab;
  }
}

/* _edit_poly_fab(): match a poly by fabrication.
*/
static u4_t
_edit_poly_fab(u4_milr m,
               u4_mold doz,
               u4_mold p_gom,
               u4_book q_gom)
{
  if ( u4_n_atom(u4_ch(q_gom)) ) {
    return _edit_poly_fab_gene(m, doz, p_gom, u4_ct(q_gom));
  }
  else {
    return _edit_poly_fab(m, doz, p_gom, u4_ch(q_gom)) &&
           _edit_poly_fab(m, doz, p_gom, u4_ct(q_gom));
  }
}

/* _edit_poly(): edit a poly.
*/
static u4_mold
_edit_poly(u4_milr m,
           u4_plox zel,
           u4_rail bar,
           u4_mold p_gom,
           u4_book q_gom)
{
  u4_lane lane = m->lane;
  u4_mold nuf;

  if ( _edit_plox_is_end(m, zel, &nuf) ) {
    return nuf;
  }
  else {
    u4_plox tig  = _edit_plox_take_hed(m, zel);
    u4_plox par  = _edit_plox_take_tal(m, zel);

    if ( !u4_n_zero(par) ) {
      return _mill_fail(m, "poly write");
    }
    else {
      u4_mold gom = u4_k_trel(lane, u4_atom_mono, p_gom, q_gom);
      u4_rail sed = _mill_slip(m, u4_noun_2, bar);
      u4_mold doz = _mill_edit(m, tig, sed, p_gom);

      if ( _mill_nest(m, doz, p_gom) ) {
        if ( _mill_nest(m, p_gom, doz) ) {
          return u4_k_trel(lane, u4_atom_mono, p_gom, q_gom);
        }
        else {
          // u4_mold jac = _mill_edit(m, zel, u4_noun_0, u4_atom_blur);
          u4_mold sut = u4_k_trel
            (lane, u4_atom_mono, 
                   u4_k_trel(lane, u4_atom_fuse, doz, p_gom),
                   q_gom);

#if 1
          if ( _edit_poly_fab(m, sut, gom, q_gom) ) {
            return sut;
          }
          else {
            return _mill_fail(m, "poly violation a");
          }
#else
          return sut;
#endif
        }
      }
      else {
        u4_mold sut = u4_k_trel(lane, u4_atom_mono, doz, q_gom);

#if 1
        if ( _edit_poly_fab(m, sut, gom, q_gom) ) {
          return sut;
        }
        else {
          return _mill_fail(m, "poly violation b");
        }
#else
          return sut;
#endif
      }
    }
  }
}

/* _edit_cell(): edit a cell.
*/
static u4_mold
_edit_cell(u4_milr m,
           u4_plox zel,
           u4_rail bar,
           u4_mold p_gom,
           u4_mold q_gom)
{
  u4_lane lane = m->lane;
  u4_mold nuf;

  if ( _edit_plox_is_end(m, zel, &nuf) ) {
    return nuf;
  }
  else {
    u4_plox tig  = _edit_plox_take_hed(m, zel);
    u4_plox par  = _edit_plox_take_tal(m, zel);
    u4_rail sed  = _mill_slip(m, u4_noun_2, bar);
    u4_rail dus  = _mill_slip(m, u4_noun_3, bar);

    return u4_k_trel
      (lane, u4_atom_cell,
             _mill_edit(m, tig, sed, p_gom),
             _mill_edit(m, par, dus, q_gom));
  }
}

/* _edit_face(): edit a face.
*/
static u4_mold
_edit_face(u4_milr m,
           u4_plox zel,
           u4_rail bar,
           u4_mark p_gom,
           u4_mold q_gom)
{
  u4_lane lane = m->lane;
  u4_mold nuf;

  if ( _edit_plox_is_end(m, zel, &nuf) ) {
    return nuf;
  }
  else {
    u4_mark cox  = u4_noun_0;
    u4_plox hom  = _edit_plox_take_tag(m, zel, &cox);

    if ( u4_n_eq(cox, p_gom) ) {
      return u4_k_trel
        (lane, u4_atom_face, p_gom, _mill_edit(m, hom, bar, q_gom));
    }
    else {
      return u4_k_trel
        (lane, u4_atom_face, p_gom, _mill_edit(m, zel, bar, q_gom));
    }
  }
}

/* _edit_forq_in()::
*/
static u4_log
_edit_forq_in(u4_milr m,
              u4_plox zel,
              u4_rail bar,
              u4_log  p_gom)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_gom) ) {
    return u4_noun_0; 
  }
  else {
    u4_mold ip_gom = u4_ch(p_gom);
    u4_mold tp_gom = u4_ct(p_gom);

    if ( _mill_cull(m, bar, ip_gom) ) {
      return _edit_forq_in(m, zel, bar, tp_gom);
    }
    else {
      return u4_k_cell
        (lane, _mill_edit(m, zel, bar, ip_gom),
               _edit_forq_in(m, zel, bar, tp_gom));
    }
  }
}

/* _edit_forq(): edit a forq.
*/
static u4_mold
_edit_forq(u4_milr m, 
           u4_plox zel,
           u4_rail bar,
           u4_log  p_gom)
{
  u4_lane lane = m->lane;
  u4_log foq = _edit_forq_in(m, zel, bar, p_gom);

  if ( u4_n_zero(foq) ) {
    return u4_atom_blot;
  }
  else return u4_k_cell(lane, u4_atom_forq, _edit_forq_in(m, zel, bar, p_gom));
}

/* _edit_mask(): edit into nothing at all.
*/
u4_mold
_edit_mask(u4_milr m,
           u4_plox zel)
{
  u4_lane lane = m->lane;
  u4_plox hom;
  u4_mark cox;
  u4_mold nuf;

  if ( u4_n_zero(zel) ) {
    return u4_atom_blur;
  }
  else if ( _edit_plox_is_end(m, zel, &nuf) ) {
    return nuf;
  }
  else if ( _edit_plox_is_tag(m, zel, &cox, &hom) ) {
    return u4_k_trel
      (lane, u4_atom_face, cox, _edit_mask(m, hom));
  }
  else {
    return u4_k_trel
      (lane, u4_atom_cell,
             _edit_mask(m, _edit_plox_take_hed(m, zel)),
             _edit_mask(m, _edit_plox_take_tal(m, zel)));
  }
}

/* _mill_edit(): edit a mold to accept a list of writes.
**
**  zel: changes: list+[list+[([%axis axis] [%term term]) mold]]
*/
u4_mold
_mill_edit(u4_milr m,
           u4_plox zel,
           u4_rail bar,
           u4_mold gom)
{
  u4_noun p_gom, q_gom;

  if ( u4_n_zero(zel) ) {
    return gom;
  }

  // %atom
  // %blot
  // %blur
  //
  if ( u4_n_eq(u4_atom_atom, gom) ||
       u4_n_eq(u4_atom_blot, gom) ||
       u4_n_eq(u4_atom_blur, gom) )
  {
    return _edit_mask(m, zel);
  }

  // [%cell p=(mold) q=(mold)]
  //
  else if ( u4_b_pq(gom, u4_atom_cell, &p_gom, &q_gom) ) {

    return _edit_cell(m, zel, bar, p_gom, q_gom);
  }

  // [%mono p=(mold) q=bush+[term mold]]
  //
  else if ( u4_b_pq(gom, u4_atom_mono, &p_gom, &q_gom) ) {
    u4_mold nuf;

    if ( _edit_plox_is_end(m, zel, &nuf) ) {
      return nuf;
    }
    else {
      u4_plox tig  = _edit_plox_take_hed(m, zel);
      u4_plox par  = _edit_plox_take_tal(m, zel);

      if ( !u4_n_zero(par) ) {
        return _mill_fail(m, "mono abuse");
      }
      else {
        u4_rail sed = _mill_slip(m, u4_noun_2, bar);
        u4_mold doz = _mill_edit(m, tig, sed, p_gom);

        if ( _mill_nest(m, doz, p_gom) ) {
          return gom;
        }
        else {
          u4_burp(m->lane, "doz", _mill_dump(m, doz));
          u4_burp(m->lane, "p_gom", _mill_dump(m, p_gom));

          return _mill_fail(m, "mono violation");
        }
      }
    }
  }

  // [%poly p=(mold) q=bush+[term mold]]
  //
  else if ( u4_b_pq(gom, u4_atom_poly, &p_gom, &q_gom) ) {
    return _edit_poly(m, zel, bar, p_gom, q_gom);
  }

  // [%cube p=noun]
  // 
  else if ( u4_b_p(gom, u4_atom_cube, &p_gom) ) {
    return _edit_mask(m, zel);
  }

  // [%face p=mark q=mold]
  //
  else if ( u4_b_pq(gom, u4_atom_face, &p_gom, &q_gom) ) {
    return _edit_face(m, zel, bar, p_gom, q_gom);
  }

  // [%forq p=(list mold)]
  //
  else if ( u4_b_p(gom, u4_atom_forq, &p_gom) ) {
    return _edit_forq(m, zel, bar, p_gom);
  }

  // [%fuse p=(mold) q=(mold)]
  //
  else if ( u4_b_pq(gom, u4_atom_fuse, &p_gom, &q_gom) ) {
    return _edit_fuse(m, zel, bar, p_gom, q_gom);
  }

  // [%hold p=(mold) q=(gene)]
  //
  else if ( u4_b_pq(gom, u4_atom_hold, &p_gom, &q_gom) ) {
    u4_noun sul = _mill_repo(m, p_gom, q_gom);
    u4_noun vaq = _mill_edit(m, zel, bar, sul);

    if ( u4_n_eq(sul, vaq) ) {
      return gom;
    }
    else return vaq;
  }
  else return u4_trip;
}
