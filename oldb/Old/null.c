/* mill/null.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_t _null_orth(u4_milr, u4_bag, u4_bag, u4_type, u4_type);
    static u4_t _null_main(u4_milr, u4_bag, u4_type);


/* _null_orth_atom(): orthogonality with atom.
*/
static u4_t
_null_orth_atom(u4_milr m,
                u4_bag  beg,
                u4_type q_lug)
{
  u4_noun pq_lug, qq_lug;

  if ( u4_n_eq(u4_atom_atom, q_lug) ) {
    return 0;
  }
  else if ( u4_n_eq(u4_atom_blur, q_lug) ) {
    return 0;
  }
  else if ( u4_b_pq(q_lug, u4_atom_bone, &pq_lug, &qq_lug) ) {
    return 1;
  }
  else if ( u4_b_p(q_lug, u4_atom_cube, &pq_lug) ) {
    if ( u4_n_atom(pq_lug) ) {
      return 0;
    } else return 1;
  }
  else if ( u4_b_pq(q_lug, u4_atom_fork, &pq_lug, &qq_lug) ) {
    return ( _null_main(m, beg, pq_lug) || 
             _null_orth_atom(m, beg, pq_lug) ) &&
           ( _null_main(m, beg, qq_lug) || 
             _null_orth_atom(m, beg, qq_lug) );
  }
  else if ( u4_b_pq(q_lug, u4_atom_fuse, &pq_lug, &qq_lug) ) {
    return _null_orth_atom(m, beg, pq_lug) ||
           _null_orth_atom(m, beg, qq_lug);
  }
  else if ( u4_b_pq(q_lug, u4_atom_post, &pq_lug, &qq_lug) ) {
    u4_type bof = _mill_repo(m, pq_lug, qq_lug);

    return _null_orth_atom(m, beg, bof);
  }
  else return _null_orth_atom(m, beg, _mill_reap(m, q_lug));
}

/* _null_orth_leaf(): orthogonality with atomic cube.
*/
static u4_t
_null_orth_leaf(u4_milr m,
                u4_bag  beg,
                u4_atom pp_lug,
                u4_type q_lug)
{
  u4_noun pq_lug, qq_lug;

  if ( u4_n_eq(u4_atom_atom, q_lug) ) {
    return 0;
  }
  else if ( u4_n_eq(u4_atom_blur, q_lug) ) {
    return 0;
  }
  else if ( u4_b_pq(q_lug, u4_atom_bone, &pq_lug, &qq_lug) ) {
    return 1;
  }
  else if ( u4_b_p(q_lug, u4_atom_cube, &pq_lug) ) {
    if ( u4_n_eq(pp_lug, pq_lug) ) {
      return 0;
    }
    else return 1;
  }
  else if ( u4_b_pq(q_lug, u4_atom_fork, &pq_lug, &qq_lug) ) {
    return ( _null_main(m, beg, pq_lug) || 
             _null_orth_leaf(m, beg, pp_lug, pq_lug) ) &&
           ( _null_main(m, beg, qq_lug) || 
             _null_orth_leaf(m, beg, pp_lug, qq_lug) );
  }
  else if ( u4_b_pq(q_lug, u4_atom_fuse, &pq_lug, &qq_lug) ) {
    return _null_orth_leaf(m, beg, pp_lug, pq_lug) ||
           _null_orth_leaf(m, beg, pp_lug, qq_lug);
  }
  else if ( u4_b_pq(q_lug, u4_atom_post, &pq_lug, &qq_lug) ) {
    u4_type bof = _mill_repo(m, pq_lug, qq_lug);

    return _null_orth_leaf(m, beg, pp_lug, bof);
  }
  else return _null_orth_leaf(m, beg, pp_lug, _mill_reap(m, q_lug));
}

/* _null_orth_bone(): orthogonality with bone.
*/
static u4_t
_null_orth_bone(u4_milr m,
                u4_bag  beg,
                u4_bag  cal,
                u4_axis pp_lug,
                u4_type qp_lug,
                u4_type q_lug)
{
  u4_lane lane = m->lane;
  u4_noun pq_lug, qq_lug;

  if ( u4_n_eq(u4_noun_1, pp_lug) ) {
    return _null_orth(m, beg, cal, qp_lug, q_lug);
  }
  else {
    if ( u4_n_eq(u4_atom_atom, q_lug) ) {
      return 1;
    }
    else if ( u4_n_eq(u4_atom_blur, q_lug) ) {
      return 0;
    }
    else if ( u4_b_pq(q_lug, u4_atom_bone, &pq_lug, &qq_lug) ) {
      while ( 1 ) {
        if ( !u4_n_eq(u4_op_tip(pp_lug), u4_op_tip(pq_lug)) ) {
          return 0;
        }
        else {
          u4_axis wer = u4_op_tap(lane, pp_lug);
          u4_axis rut = u4_op_tap(lane, pq_lug);

          if ( u4_n_eq(u4_noun_1, rut) ) {
            return _null_orth_bone(m, beg, cal, wer, qp_lug, qq_lug);
          }
          else if ( u4_n_eq(u4_noun_1, wer) ) {
            return _null_orth
              (m, beg, cal, qp_lug, u4_k_trel(lane, u4_atom_bone, rut, qq_lug));
          }
          else {
            pp_lug = wer;
            pq_lug = rut;
          }
        }
      }
    }
    else if ( u4_b_p(q_lug, u4_atom_cube, &pq_lug) ) {
      if ( u4_n_atom(pq_lug) ) {
        return 1;
      }
      return _null_orth_bone
        (m, beg, cal, pp_lug, qp_lug, _mill_reap(m, q_lug));
    }
    else if ( u4_b_pq(q_lug, u4_atom_fork, &pq_lug, &qq_lug) ) {
      return ( _null_main(m, beg, pq_lug) ||
               _null_orth_bone(m, beg, cal, pp_lug, qp_lug, pq_lug) ) &&
             ( _null_main(m, beg, qq_lug) ||
               _null_orth_bone(m, beg, cal, pp_lug, qp_lug, qq_lug) );
    }
    else if ( u4_b_pq(q_lug, u4_atom_fuse, &pq_lug, &qq_lug) ) {
      return _null_orth_bone(m, beg, cal, pp_lug, qp_lug, pq_lug) ||
             _null_orth_bone(m, beg, cal, pp_lug, qp_lug, qq_lug);
    }
    else if ( u4_b_pq(q_lug, u4_atom_post, &pq_lug, &qq_lug) ) {
      u4_noun p_lug = u4_k_trel(lane, u4_atom_bone, pp_lug, qp_lug);
      u4_type bap   = _mill_repo(m, pq_lug, qq_lug);
      u4_noun fum   = u4_k_cell(lane, p_lug, bap);

      if ( u4_bag_in(fum, cal) ) {
        return 1;
      }
      else {
        cal = u4_bag_add(lane, fum, cal);

        return _null_orth_bone(m, beg, cal, pp_lug, qp_lug, bap);
      }
    }
    else {
      return _null_orth_bone
          (m, beg, cal, pp_lug, qp_lug, _mill_reap(m, q_lug));
    }
  }
}

/* _null_orth(): check orthogonality of p_lug and q_lug, each non-null.
*/
static u4_t
_null_orth(u4_milr m,
           u4_bag  beg,
           u4_bag  cal,
           u4_type p_lug,
           u4_type q_lug)
{
  u4_lane lane = m->lane;
  u4_noun pp_lug, qp_lug;

  // u4_err(m->lane, "no: p_lug", p_lug);
  // u4_err(m->lane, "no: q_lug", q_lug);

  if ( u4_n_eq(u4_atom_atom, p_lug) ) {
    return _null_orth_atom(m, beg, q_lug);
  }
  else if ( u4_n_eq(u4_atom_blur, p_lug) ) {
    return 0;
  }
  else if ( u4_b_pq(p_lug, u4_atom_bone, &pp_lug, &qp_lug) ) {
    return _null_orth_bone(m, beg, cal, pp_lug, qp_lug, q_lug);
  }
  else if ( u4_b_p(p_lug, u4_atom_cube, &pp_lug) ) {
    if ( u4_n_atom(pp_lug) ) {
      return _null_orth_leaf(m, beg, pp_lug, q_lug);
    }
    else return _null_orth(m, beg, cal, _mill_reap(m, p_lug), q_lug);
  }
  else if ( u4_b_pq(p_lug, u4_atom_fork, &pp_lug, &qp_lug) ) {
    return ( ( _null_main(m, beg, pp_lug) ||
               _null_orth(m, beg, cal, pp_lug, q_lug) ) &&
             ( _null_main(m, beg, qp_lug) ||
               _null_orth(m, beg, cal, qp_lug, q_lug) ) );
  }
  else if ( u4_b_pq(p_lug, u4_atom_fuse, &pp_lug, &qp_lug) ) {
    return _null_orth(m, beg, cal, pp_lug, q_lug) ||
           _null_orth(m, beg, cal, qp_lug, q_lug);
  }
  else if ( u4_b_pq(p_lug, u4_atom_post, &pp_lug, &qp_lug) ) {
    u4_type gis = _mill_repo(m, pp_lug, qp_lug);
    u4_noun fum = u4_k_cell(lane, gis, q_lug);

    if ( u4_bag_in(fum, cal) ) {
      return 1;
    }
    else {
      cal = u4_bag_add(lane, fum, cal);

      return _null_orth(m, beg, cal, gis, q_lug);
    }
  }
  else return _null_orth(m, beg, cal, _mill_reap(m, p_lug), q_lug);
}

/* _null_main(): null, with recursion control.
*/
static u4_t
_null_main(u4_milr m,
           u4_bag  beg,
           u4_type lug)
{
  u4_lane lane = m->lane;
  u4_noun p_lug, q_lug;

  if ( u4_n_eq(u4_atom_atom, lug) ) {
    return 0;
  }
  else if ( u4_n_eq(u4_atom_blot, lug) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_blur, lug) ) {
    return 0;
  }
  else if ( u4_b_pq(lug, u4_atom_bone, &p_lug, &q_lug) ) {
    return _null_main(m, beg, q_lug);
  }
  else if ( u4_b_p(lug, u4_atom_cube, &p_lug) ) {
    return 0;
  }
  else if ( u4_b_pq(lug, u4_atom_fork, &p_lug, &q_lug) ) {
    return _null_main(m, beg, p_lug) && _null_main(m, beg, q_lug);
  }
  else if ( u4_b_pq(lug, u4_atom_fuse, &p_lug, &q_lug) ) {
    u4_bag cal = u4_bag_add(lane, u4_k_cell(lane, p_lug, q_lug), u4_noun_0);

    return _null_main(m, beg, p_lug) ||
           _null_main(m, beg, q_lug) ||
           _null_orth(m, beg, cal, p_lug, q_lug);
  }
  else if ( u4_b_pq(lug, u4_atom_post, &p_lug, &q_lug) ) {
    u4_type dof = _mill_repo(m, p_lug, q_lug);

    if ( u4_bag_in(dof, beg) ) {
      /* We can do this because we're searching conservatively.
      */
      return 1;
    }
    else return _null_main
      (m, u4_bag_add(m->lane, dof, beg), _mill_repo(m, p_lug, q_lug));
  }
  else return _null_main(m, beg, _mill_reap(m, lug));
}

/* _mill_null(): true if type is empty.
*/
u4_t
_mill_null(u4_milr m,
           u4_type lug)
{
  u4_nopt reb = u4_tab_get(lug, m->dam);

  if ( reb != u4_bull ) {
    return u4_n_zero(reb);
  }
  else {
    u4_t t_reb = _null_main(m, u4_bag_add(m->lane, lug, u4_noun_0), lug);

    m->dam = u4_tab_add(m->lane, lug, (t_reb ? u4_noun_0 : u4_noun_1), m->dam);
    return t_reb;
  }
}
