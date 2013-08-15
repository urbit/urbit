/* watt/dump.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

    /** Global variables.
    **/
      u4_pool how;

      static u4_prep
      _dump_main(u4_plow p, u4_pool gil, u4_pool nip, u4_type typ);

/* seal:dump (fake)
*/
  static u4_pool
  _dump_seal_main(u4_plow p,
                  u4_pool gil,
                  u4_type typ)
  {
    u4_lane lan = p->lan;
    u4_noun p_typ, q_typ;

    // %atom
    // %blur
    //
    if ( u4_n_eq(u4_atom_atom, typ) ||
         u4_n_eq(u4_atom_blur, typ) ||
         u4_n_eq(u4_atom_blot, typ) ) 
    {
      return u4_nul;
    }

    // [%cell p=type q=type]
    //
    else if ( u4_b_pq(typ, u4_atom_cell, &p_typ, &q_typ) ) {
      return u4_bag_cat(lan, _dump_seal_main(p, gil, p_typ),
                             _dump_seal_main(p, gil, q_typ));
    }

    // [%core p=type q=*]
    //
    else if ( u4_b_pq(typ, u4_atom_core, &p_typ, &q_typ) ) {
      return _dump_seal_main(p, gil, p_typ);
    }

    // [%cube p=noun]
    // 
    else if ( u4_b_p(typ, u4_atom_cube, &p_typ) ) {
      return u4_nul;
    }

    // [%face p=mark q=type]
    //
    else if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
      return _dump_seal_main(p, gil, q_typ);
    }

    // [%fork p=type q=type]
    //
    else if ( u4_b_pq(typ, u4_atom_fork, &p_typ, &q_typ) ) {
      return u4_bag_cat(lan, _dump_seal_main(p, gil, p_typ),
                              _dump_seal_main(p, gil, q_typ));
    }

    // [%fuse p=type q=type]
    //
    else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
      return u4_bag_cat(lan, _dump_seal_main(p, gil, p_typ),
                              _dump_seal_main(p, gil, q_typ));
    }

    // [%hold p=type q=gene]
    //
    else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
      if ( u4_bag_in(typ, gil) ) {
        return u4_k_trel(lan, typ, u4_nul, u4_nul);
      } 
      else {
        return _dump_seal_main
          (p, u4_bag_add(lan, typ, gil),
              _rose_repo(p, p_typ, q_typ));
      }
    }
    else {
      if ( !u4_n_atom(typ) && !u4_n_atom(u4_ch(typ)) ) {
        printf("hm!\n");
      }
      else u4_err(lan, "odd type", typ);


      return u4_trip;
    }
  }

/* _dump_seal(): produce set of holds sealing [typ].
*/
u4_pool
_dump_seal(u4_plow p,
           u4_type typ)
{
  return _dump_seal_main(p, u4_nul, typ);
}

/* _dump_cell()::
*/
static u4_noun
_dump_cell(u4_plow p,
           u4_pool gil,
           u4_pool nip,
           u4_noun p_typ,
           u4_noun q_typ)
{
  u4_lane lan = p->lan;
  u4_noun pq_typ, qq_typ;

  if ( u4_b_pq(q_typ, u4_atom_cell, &pq_typ, &qq_typ) ) {
    return u4_k_cell
      (lan, _dump_main(p, gil, nip, p_typ),
            _dump_cell(p, gil, nip, pq_typ, qq_typ));
  }
  else {
    return u4_k_trel
      (lan, _dump_main(p, gil, nip, p_typ),
             _dump_main(p, gil, nip, q_typ),
             u4_nul);
  }
}

/* _dump_core(): dump a core.
*/
static u4_prep
_dump_core(u4_plow p,
           u4_pool gil,
           u4_pool nip,
           u4_type myt,
           u4_atom kic,
           u4_spec dab)
{
  u4_lane lan = p->lan;
  u4_prep gum;
  u4_prep zib;
  u4_prep bor;

  zib = _dump_main(p, gil, nip, myt);
  bor = u4_k_atom_cat
    (lan, (u4_n_eq(u4_atom_soft, kic) ? u4_cod_in('-') : u4_cod_in('+')),
           u4_k_atom_cat
             (lan, u4_prep_decimal(lan, _gull_size(p, dab)),
                    u4_k_atom_cat
                      (lan, u4_cod_in('.'),
                             u4_prep_hexinal
                                (lan, u4_cod_in(u4_n_nub(dab) & 0xffff)))));

  gum = u4_prep_close
    (lan, '<', '>', u4_k_list(lan, zib, bor, 0));

  return gum;
}

/* _dump_fork(): dump fork as list.
*/
static u4_noun
_dump_fork(u4_plow p,
           u4_pool gil,
           u4_pool nip,
           u4_type p_typ,
           u4_type q_typ)
{
  u4_lane lan = p->lan;
  u4_noun pq_typ, qq_typ;

  if ( u4_b_pq(q_typ, u4_atom_fork, &pq_typ, &qq_typ) ) {
    return u4_kc
      (lan, _dump_main(p, gil, nip, p_typ),
            _dump_fork(p, gil, nip, pq_typ, qq_typ));
  }
  else {
    return u4_kt
      (lan, _dump_main(p, gil, nip, p_typ),
            _dump_main(p, gil, nip, q_typ),
            u4_nul);
  }
}
 
/* _dump_main(): dump with gil and nip.
*/
static u4_prep
_dump_main(u4_plow p,
           u4_pool gil,
           u4_pool nip,
           u4_type typ)
{
  u4_lane lan = p->lan;
  u4_noun p_typ, q_typ, r_typ, s_typ;

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
      (lan, '[', ']', 
              _dump_cell(p, gil, nip, p_typ, q_typ));
  }
  else if ( u4_b_pqrs(typ, u4_atom_core, &p_typ, &q_typ, &r_typ, &s_typ) ) {
    return _dump_core(p, gil, nip, p_typ, q_typ, s_typ);
  }
  else if ( u4_b_p(typ, u4_atom_cube, &p_typ) ) {
    if ( u4_n_zero(p_typ) ) {
      return u4_cod_in('~');
    }
    else if ( u4_n_atom(p_typ) ) {
      return u4_k_atom_cat
        (lan, u4_cod_in('%'), u4_prep_textual(lan, p_typ));
    }
    else {
      return u4_k_list
        (lan,
         u4_atom_nail,
         u4_cod_in('%'),
         u4_pump_prep(lan, p_typ),
         0);
    }
  }
  else if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
    return u4_k_list
      (lan, u4_atom_nail, 
             u4_prep_textual(lan, p_typ),
             u4_cod_in(':'),
             _dump_main(p, gil, nip, q_typ),
             0);
  }
  else if ( u4_b_pq(typ, u4_atom_fork, &p_typ, &q_typ) ) {
    u4_noun pip_typ, piq_typ;

    if ( u4_b_p(p_typ, u4_atom_cube, &pip_typ) &&
         u4_b_p(q_typ, u4_atom_cube, &piq_typ) &&
         u4_n_eq(u4_yes, pip_typ) &&
         u4_n_eq(u4_no, piq_typ) )
    {
      return u4_cod_in('?');
    }
    else {
      return
        u4_prep_close
          (lan, '{', '}', _dump_fork(p, gil, nip, p_typ, q_typ));
    }
  }
  else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
    return u4_prep_close
      (lan, '(', ')', 
             u4_k_list(lan,
                       _dump_main(p, gil, nip, p_typ),
                       _dump_main(p, gil, nip, q_typ),
                       0));
  }
  else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
    if ( u4_bag_in(typ, gil) ) {
      u4_noun fez = u4_k_atom_cat
          (lan, u4_cod_in('$'), 
                 u4_prep_decimal(lan, u4_bag_at(lan, typ, u4_axis_1, gil)));

      if ( u4_bag_in(typ, nip) || u4_bag_in(typ, how) ) {
        return fez;
      }
      else {
        how = u4_bag_add(lan, typ, how);
        { 
          u4_prep hod = 
            _dump_main(p, gil, 
                          u4_bag_add(lan, typ, nip),
                          _rose_repo(p, p_typ, q_typ));

          
          return u4_k_list
            (lan, u4_atom_nail, 
                   fez,
                   u4_cod_in('='),
                   hod,
                   0);
        }
      }
    }
    else {
      return _dump_main(p, gil, nip, _rose_repo(p, p_typ, q_typ));
    }
  }
  else {
    return u4_trip;
  }
}

/* _durb_in()::
*/
static u4_noun
_durb_in(u4_plow p,
         u4_rail bar)
{
  if ( u4_n_zero(bar) ) {
    return u4_nul;
  } else {
    return u4_k_cell(p->lan, _dump_type(p, u4_ch(bar)), 
                             _durb_in(p, u4_ct(bar)));
  }
}

/* _dump_durb(): prepare rail for printing.
*/
u4_prep
_dump_durb(u4_plow p,
           u4_rail bar)
{
  if ( u4_n_zero(bar) ) {
    return u4_cod_in('~');
  }
  else {
    return u4_prep_close
      (p->lan, '(', ')', _durb_in(p, bar));
  }
}

/* _dump_type(): prepare type for printing.
*/
u4_prep
_dump_type(u4_plow p,
           u4_type typ)
{
  u4_pool gil = _dump_seal(p, typ);
  u4_prep par;

  // Lamentable.
  //
  how = u4_nul;
  par = _dump_main(p, gil, u4_nul, typ);
  how = u4_nul;

  return par;
}

/* _dump_size()::
*/
u4_atom
_dump_size(u4_lane lan, u4_noun box)
{
  if ( u4_n_atom(box) ) {
    return u4_math_1;
  }
  else {
    return u4_op_add
      (lan, _dump_size(lan, u4_ch(box)), 
            _dump_size(lan, u4_ct(box)));
  }
}
