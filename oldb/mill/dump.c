/* mill/dump.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

    /** Global variables.
    **/
      u4_bag how;

    /** Forward declarations.
    **/
      static u4_prep
      _dump_main(u4_milr, u4_bag, u4_bag, u4_mold);


/* _dump_count()::
*/
static u4_atom
_dump_count(u4_lane lane, u4_noun q_typ)
{
  if ( u4_n_atom(u4_ch(q_typ)) ) {
    return u4_noun_1;
  }
  else {
    return u4_op_add
      (lane, _dump_count(lane, u4_ch(q_typ)), 
             _dump_count(lane, u4_ct(q_typ)));
  }
}

/* _dump_cell()::
*/
static u4_noun
_dump_cell(u4_milr m,
           u4_bag  gil,
           u4_bag  nip,
           u4_noun p_typ,
           u4_noun q_typ)
{
  u4_lane lane = m->lane;
  u4_noun pq_typ, qq_typ;

  if ( u4_b_pq(q_typ, u4_atom_cell, &pq_typ, &qq_typ) ) {
    return u4_k_cell
      (lane, _dump_main(m, gil, nip, p_typ),
             _dump_cell(m, gil, nip, pq_typ, qq_typ));
  }
  else {
    return u4_k_trel
      (lane, _dump_main(m, gil, nip, p_typ),
             _dump_main(m, gil, nip, q_typ),
             u4_noun_0);
  }
}

/* _dump_mono(): dump a mono.
*/
static u4_prep
_dump_mono(u4_milr m,
           u4_bag  gil,
           u4_bag  nip,
           u4_mold p_typ,
           u4_book q_typ)
{
  u4_lane lane = m->lane;
  u4_prep gum;
  u4_prep zib;
  u4_prep bor;

  zib = _dump_main(m, gil, nip, p_typ);
  bor = u4_k_atom_cat
    (lane, u4_cod_in('+'),
           u4_k_atom_cat
             (lane, u4_prep_decimal(lane, _dump_count(lane, q_typ)),
                    u4_k_atom_cat
                      (lane, u4_cod_in('.'),
                             u4_prep_hexinal
                                (lane, u4_cod_in(u4_n_nub(q_typ) & 0xffff)))));

  gum = u4_prep_close
    (lane, '<', '>', u4_k_list(lane, zib, bor, 0));

  return gum;
}

/* _dump_poly(): dump a poly.
*/
static u4_prep
_dump_poly(u4_milr m,
           u4_bag  gil,
           u4_bag  nip,
           u4_mold p_typ,
           u4_book q_typ)
{
  u4_lane lane = m->lane;
  u4_prep gum;
  u4_prep zib;
  u4_prep bor;

  zib = _dump_main(m, gil, nip, p_typ);
  bor = u4_k_atom_cat
    (lane, u4_cod_in('-'),
           u4_k_atom_cat
             (lane, u4_prep_decimal(lane, _dump_count(lane, q_typ)),
                    u4_k_atom_cat
                      (lane, u4_cod_in('.'),
                             u4_prep_hexinal
                                (lane, u4_cod_in(u4_n_nub(q_typ) & 0xffff)))));

  gum = u4_prep_close
    (lane, '<', '>', u4_k_list(lane, zib, bor, 0));

  return gum;
}

/* _dump_forq(): produce a dump list of forqs.
*/
static u4_noun
_dump_forq(u4_milr m,
           u4_bag  gil,
           u4_bag  nip,
           u4_log  p_typ)
{
  if ( u4_n_zero(p_typ) ) {
    return u4_noun_0;
  }
  else return 
    u4_k_cell(m->lane, _dump_main(m, gil, nip, u4_ch(p_typ)),
                       _dump_forq(m, gil, nip, u4_ct(p_typ)));
}

/* _dump_main(): dump with gil and nip.
*/
static u4_prep
_dump_main(u4_milr m,
           u4_bag  gil,
           u4_bag  nip,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_noun p_typ, q_typ;

  if ( u4_n_atom(typ) ) {
    if ( u4_n_eq(u4_atom_atom, typ) ) {
      return u4_cod_in('@');
    }
    else if ( u4_n_eq(u4_atom_blot, typ) ) {
      return u4_cod_in('#');
    }
    else if ( u4_n_eq(u4_atom_blur, typ) ) {
      return u4_cod_in('*');
    }
    else return u4_trip;
  }
  else if ( u4_b_pq(typ, u4_atom_cell, &p_typ, &q_typ) ) {
    return u4_prep_close
      (lane, '[', ']', 
              _dump_cell(m, gil, nip, p_typ, q_typ));
  }

  else if ( u4_b_pq(typ, u4_atom_mono, &p_typ, &q_typ) ) {
    return _dump_mono(m, gil, nip, p_typ, q_typ);
  }

  else if ( u4_b_pq(typ, u4_atom_poly, &p_typ, &q_typ) ) {
    return _dump_poly(m, gil, nip, p_typ, q_typ);
  }

  else if ( u4_b_p(typ, u4_atom_cube, &p_typ) ) {
    if ( u4_n_zero(p_typ) ) {
      return u4_cod_in('~');
    }
    else if ( u4_n_atom(p_typ) ) {
      return u4_k_atom_cat
        (lane, u4_cod_in('%'), u4_prep_textual(lane, p_typ));
    }
    else {
      return u4_k_list
        (lane,
         u4_atom_nail,
         u4_cod_in('%'),
         u4_pump_prep(lane, p_typ),
         0);
    }
  }

  else if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
    return u4_k_list
      (lane, u4_atom_nail, 
             u4_prep_textual(lane, p_typ),
             u4_cod_in(':'),
             _dump_main(m, gil, nip, q_typ),
             0);
  }

  else if ( u4_b_p(typ, u4_atom_forq, &p_typ) ) {
    u4_noun pip_typ, pitp_typ;

    if ( (u4_log_len(p_typ) == 2) &&
         u4_b_p(u4_ch(p_typ), u4_atom_cube, &pip_typ) &&
         u4_b_p(u4_ch(u4_ct(p_typ)), u4_atom_cube, &pitp_typ) &&
         u4_n_eq(u4_noun_0, pip_typ) &&
         u4_n_eq(u4_noun_1, pitp_typ) )
    {
      return u4_cod_in('?');
    }
    else {
      return
        u4_prep_close
          (lane, '{', '}', _dump_forq(m, gil, nip, p_typ));
    }
  }

  else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
    return u4_prep_close
      (lane, '(', ')', 
             u4_k_list(lane,
                       _dump_main(m, gil, nip, p_typ),
                       _dump_main(m, gil, nip, q_typ),
                       0));
  }

  else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
    if ( u4_bag_in(typ, gil) ) {
      u4_noun fez = u4_k_atom_cat
          (lane, u4_cod_in('$'), 
                 u4_prep_decimal(lane, u4_bag_at(lane, typ, u4_noun_1, gil)));

      if ( u4_bag_in(typ, nip) || u4_bag_in(typ, how) ) {
        return fez;
      }
      else {
        how = u4_bag_add(lane, typ, how);
        { 
          u4_prep hod = 
            _dump_main(m, gil, 
                          u4_bag_add(lane, typ, nip),
                          _mill_repo(m, p_typ, q_typ));

          
          return u4_k_list
            (lane, u4_atom_nail, 
                   fez,
                   u4_cod_in('='),
                   hod,
                   0);
        }
      }
    }
    else {
      return _dump_main(m, gil, nip, _mill_repo(m, p_typ, q_typ));
    }
  }

  else {
    return u4_trip;
  }
}

/* _durb_in()::
*/
static u4_noun
_durb_in(u4_milr m,
         u4_rail bar)
{
  if ( u4_n_zero(bar) ) {
    return u4_noun_0;
  } else {
    return u4_k_cell(m->lane, _mill_dump(m, u4_ch(bar)), 
                              _durb_in(m, u4_ct(bar)));
  }
}

/* _mill_durb(): prepare rail for printing.
*/
u4_prep
_mill_durb(u4_milr m,
           u4_rail bar)
{
  return u4_prep_close
    (m->lane, '(', ')', _durb_in(m, bar));
}

/* _mill_dump(): prepare mold for printing.
*/
u4_prep
_mill_dump(u4_milr m,
           u4_mold typ)
{
  u4_bag gil = _mill_seal(m, typ);
  u4_prep par;

  // Lamentable.
  //
  how = u4_noun_0;
  par = _dump_main(m, gil, u4_noun_0, typ);
  how = u4_noun_0;

  return par;
}
