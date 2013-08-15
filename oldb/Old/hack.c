/* mill/hack.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _hack_main(): hack with mask.
*/
static u4_type
_hack_main(u4_milr m,
           u4_bag  lav,
           u4_log  bod,
           u4_axis fis,
           u4_type bun)
{
  u4_lane lane = m->lane;
  u4_noun p_bun, q_bun;

  if ( u4_n_eq(u4_noun_1, fis) ) {
    return bun;
  }
  else {
    if ( u4_n_eq(u4_atom_atom, bun) ) {
      return u4_atom_blot;
    }
    else if ( u4_n_eq(u4_atom_blot, bun) ) {
      return u4_atom_blot;
    }
    else if ( u4_n_eq(u4_atom_blur, bun) ) {
      return u4_atom_blur;
    }

    else if ( u4_b_pq(bun, u4_atom_bone, &p_bun, &q_bun) ) {
      u4_axis sud = u4_op_tip(fis);

      if ( !u4_n_eq(sud, u4_op_tip(p_bun)) ) {
        return u4_atom_blur;
      }
      else {
        fis = u4_op_tap(lane, fis);
        p_bun = u4_op_tap(lane, p_bun);
        bod = _mill_slip(m, sud, bod);

        if ( u4_n_eq(u4_noun_1, p_bun) ) {
          return _hack_main(m, u4_noun_0, bod, fis, q_bun);
        }
        else {
          u4_type rit = u4_k_trel(lane, u4_atom_bone, p_bun, q_bun);

          return _hack_main(m, u4_noun_0, bod, fis, rit);
        }
      }
    }

    else if ( u4_b_p(bun, u4_atom_cube, &p_bun) ) {
      if ( u4_n_atom(p_bun) ) {
        return u4_atom_blot;
      }
      else return _hack_main(m, lav, bod, fis, _mill_reap(m, bun));
    }

    else if ( u4_b_pq(bun, u4_atom_fork, &p_bun, &q_bun) ) {
      if ( _mill_cull(m, p_bun, bod) ) {
        return _hack_main(m, lav, bod, fis, q_bun);
      }
      else if ( _mill_cull(m, q_bun, bod) ) {
        return _hack_main(m, lav, bod, fis, p_bun);
      }
      else {
        return _mill_eith
          (m, _hack_main(m, lav, bod, fis, p_bun), 
              _hack_main(m, lav, bod, fis, q_bun));
      }
    }

    else if ( u4_b_pq(bun, u4_atom_fuse, &p_bun, &q_bun) ) {
      return _mill_both
        (m, _hack_main(m, lav, bod, fis, p_bun),
            _hack_main(m, lav, u4_k_cell(lane, p_bun, bod), fis, q_bun));
    }

    else if ( u4_b_pq(bun, u4_atom_post, &p_bun, &q_bun) ) {
      u4_type cuv = _mill_pull(m, bod, bun);

      if ( u4_bag_in(cuv, lav) ) {
        return u4_atom_blot;
      }
      else {
        lav = u4_bag_add(m->lane, cuv, lav);

        return _hack_main(m, lav, bod, fis, _mill_repo(m, p_bun, q_bun));
      }
    }      

    else {
      return _hack_main(m, lav, bod, fis, _mill_reap(m, bun));
    }
  }
}

/* _mill_hack(): cut type.
*/
u4_type
_mill_hack(u4_milr m,
           u4_axis fis,
           u4_type bun)
{
  if ( _mill_null(m, bun) ) {
    return u4_atom_blot;
  }
  else {
    u4_type foz = _hack_main(m, u4_noun_0, u4_noun_0, fis, bun);
     
    // return _mill_flay(m, foz);
    return foz;
  }
}
