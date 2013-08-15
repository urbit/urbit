/* mill/gene.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _gene_book(): normalize a book.
*/
static u4_noun
_gene_book(u4_milr m,
           u4_lump beq)
{
  if ( u4_n_cell(u4_ch(beq)) ) {
    return u4_k_cell
      (m->lane, _gene_book(m, u4_ch(beq)), _gene_book(m, u4_ct(beq)));
  }
  else {
    return u4_k_cell(m->lane, u4_ch(beq), _mill_gene(m, u4_ct(beq)));
  }
}

/* _gene_mute(): normalize a mutation list.
*/
static u4_noun
_gene_mute(u4_milr m,
           u4_lump fam)
{
  if ( u4_n_zero(fam) ) {
    return fam;
  }
  else {
    u4_noun cax = u4_ch(fam);
    u4_noun leb = u4_ct(fam);

    return u4_k_cell
      (m->lane, u4_k_cell(m->lane, _mill_gene(m, u4_ch(cax)),
                                   _mill_gene(m, u4_ct(cax))),
                _gene_mute(m, leb));
  }
}

#if 0
  /** This is code for the potential find replacement.
  **/
/* _gene_drop(): normalize a drop.
*/
static u4_noun
_gene_drop(u4_milr m,
           u4_lump wex)
{
  u4_noun p_wex, q_wex;

  if ( u4_b_pq(wex, u4_atom_port, &p_wex, &q_wex) ) {
    if ( u4_n_atom(p_wex) && u4_n_atom(q_wex) ) {
      return wex;
    } else return _mill_fail(m, "invalid gene");
  }
  else if ( u4_b_p(wex, u4_atom_port, &p_wex) ) {
    if ( u4_n_atom(p_wex) ) {
      return wex;
    } else return _mill_fail(m, "invalid gene");
  }
  else if ( u4_b_p(wex, u4_atom_limb, &p_wex) ) {
    if ( u4_n_atom(p_wex) ) {
      return wex;
    } else return _mill_fail(m, "invalid gene");
  }
  else return _mill_fail(m, "invalid gene");
}

/* _gene_path(): normalize a path.
*/
static u4_noun
_gene_path(u4_milr m,
           u4_lump dar)
{
  if ( u4_n_zero(dar) ) {
    return dar;
  } else {
    return u4_k_cell
      (m->lane, _gene_drop(m, u4_ch(dar)), _gene_path(m, u4_ct(dar)));
  }
}

/* _gene_coat(): normalize a change list.
*/
static u4_noun
_gene_coat(u4_milr m,
           u4_lump ruf)
{
  if ( u4_n_zero(ruf) ) {
    return ruf;
  } else {
    u4_noun cus = u4_ch(ruf);

    return
      u4_k_cell(m->lane, 
                u4_k_cell(m->lane, _gene_path(m, u4_ch(cus)), 
                                   _mill_gene(m, u4_ct(cus))),
                _gene_coat(m, u4_ct(ruf)));
  }
}
#endif

/* _mill_gene(): lump to gene.
*/
u4_gene
_mill_gene(u4_milr m,
           u4_lump rad)
{
  u4_lane lane  = m->lane;
  u4_noun p_rad, q_rad, r_rad;

  if ( u4_b_fork(rad, &p_rad, &q_rad) ) {
    return u4_k_cell(lane, _mill_gene(m, p_rad), _mill_gene(m, q_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_bend, &p_rad, &q_rad) ) {
    return u4_k_trel
      (lane, u4_atom_bend, _gene_mute(m, p_rad), _mill_gene(m, q_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_cast, &p_rad, &q_rad) ) {
    return u4_k_trel
      (lane, u4_atom_cast, _mill_gene(m, p_rad), _mill_gene(m, q_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_coat, &p_rad, &q_rad) ) {
    return u4_k_trel
      (lane, u4_atom_coat, p_rad, _mill_gene(m, q_rad));
  }

  else if ( u4_b_p(rad, u4_atom_dbug, &p_rad) ) {
    return u4_k_cell(lane, u4_atom_dbug, _mill_gene(m, p_rad));
  }
#if 0
  else if ( u4_b_pq(rad, u4_atom_find, &p_rad, &q_rad) ) {
    return u4_k_cell(lane, _gene_path(m, p_rad), _gene_coat(m, q_rad));
  }
#endif
  else if ( u4_b_pqr(rad, u4_atom_if, &p_rad, &q_rad, &r_rad) ) {
    return u4_k_qual
      (lane, u4_atom_if,
             _mill_gene(m, p_rad),
             _mill_gene(m, q_rad),
             _mill_gene(m, r_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_like, &p_rad, &q_rad) ) {
    return u4_k_trel
      (lane, u4_atom_like, _mill_gene(m, p_rad), _mill_gene(m, q_rad));
  }

  else if ( u4_b_p(rad, u4_atom_limb, &p_rad) ) {
    if ( u4_n_cell(p_rad) ) {
      goto bad;
    }
    else return rad;
  }

  else if ( u4_b_pq(rad, u4_atom_link, &p_rad, &q_rad)) {
    return u4_k_trel
      (lane, u4_atom_link, _mill_gene(m, p_rad), _mill_gene(m, q_rad));
  }

  else if ( u4_b_p(rad, u4_atom_load, &p_rad) ) {
    return u4_k_cell(lane, u4_atom_load, _gene_book(m, p_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_look, &p_rad, &q_rad) ) {
    if ( !u4_n_atom(p_rad) ) {
      goto bad;
    }
    else return rad;
  }

  else if ( u4_b_pq(rad, u4_atom_raw, &p_rad, &q_rad) ) {
    if ( u4_n_eq(u4_noun_3, p_rad) ||
         u4_n_eq(u4_noun_4, p_rad) ||
         u4_n_eq(u4_noun_5, p_rad) ||
         u4_n_eq(u4_noun_6, p_rad) )
    {
      return u4_k_trel(lane, u4_atom_raw, p_rad, _mill_gene(m, q_rad));
    }
    else goto bad;
  }

  else if ( u4_b_p(rad, u4_atom_rock, &p_rad) ) {
    return rad;
  }

  else if ( u4_b_pq(rad, u4_atom_site, &p_rad, &q_rad) ) {
    return u4_k_trel(lane, u4_atom_site, p_rad, _mill_gene(m, q_rad));
  }

  else if ( u4_b_pq(rad, u4_atom_spot, &p_rad, &q_rad) ) {
    return u4_k_trel(lane, u4_atom_spot, p_rad, _mill_gene(m, q_rad));
  }

  else if ( u4_b_pqr(rad, u4_atom_sure, &p_rad, &q_rad, &r_rad) ) {
    return u4_k_qual
      (lane, u4_atom_sure, 
             _mill_gene(m, p_rad), 
             _mill_gene(m, q_rad),
             _mill_gene(m, r_rad));
  }

  else {
    u4_noun fos = _mill_open(m, rad);

    if ( u4_n_eq(fos, rad) ) {
      goto bad;
    }
    else {
      if ( u4_n_eq(fos, _mill_gene(m, fos)) ) {
        return rad;
      }
      else {
        u4_err(lane, "fos", fos);
        u4_err(lane, "naf", _mill_gene(m, fos));
        goto bad;
      }
    }
  }

  bad: {
    u4_burp(lane, "gene", u4_pump_prep(lane, rad));
   
    return _mill_fail(m, "invalid gene");
  }
}
