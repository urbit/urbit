/* mill/flay.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _flay_main(): flay, with loop control.
*/
static u4_type
_flay_main(u4_milr m,
           u4_bag  hed,
           u4_type naf)
{
  u4_noun p_naf, q_naf;

  if (   u4_n_atom(naf)
      || u4_b_p(naf, u4_atom_cube, 0) 
      || u4_b_pq(naf, u4_atom_bone, &p_naf, &q_naf) 
      || u4_b_pq(naf, u4_atom_cone, &p_naf, &q_naf) 
      || u4_b_pq(naf, u4_atom_pair, &p_naf, &q_naf) )
  {
    return naf;
  }
  else if ( u4_b_pq(naf, u4_atom_fork, &p_naf, &q_naf) ) {
    return _mill_eith(m, _flay_main(m, hed, p_naf), 
                         _flay_main(m, hed, q_naf));
  }
  else if ( u4_b_pq(naf, u4_atom_fuse, &p_naf, &q_naf) ) {
    return _mill_both(m, _flay_main(m, hed, p_naf), 
                         _flay_main(m, hed, q_naf));
  }
  else if ( u4_b_pq(naf, u4_atom_post, &p_naf, &q_naf) ) {
    if ( u4_bag_in(naf, hed) ) {
      return naf;
    }
    else {
      hed = u4_bag_add(m->lane, naf, hed);
      {
        u4_type tey = _mill_repo(m, p_naf, q_naf);
        u4_type mal = _flay_main(m, hed, tey);
   
        return (u4_n_eq(tey, mal) ? naf : mal);
      }
    }
  }
  else {
    return _flay_main(m, hed, _mill_reap(m, naf));
  }

}

/* _mill_flay(): strip decorations.
*/
u4_type
_mill_flay(u4_milr m,
           u4_type naf)
{
  return _flay_main(m, u4_noun_0, naf);
}
