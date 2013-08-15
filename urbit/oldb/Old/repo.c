/* mill/repo.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_repo(): replay post.
*/
u4_type
_mill_repo(u4_milr m,
           u4_type bir,
           u4_gene fug)
{
  u4_lane lane = m->lane;
  u4_noun fad = u4_k_cell(lane, bir, fug);
  u4_noun fan;
  u4_type pol;

  if ( u4_bag_in(fad, m->fan) ) {
    return _mill_fail(m, "inference recursion");
  }

  fan = m->fan;
  m->fan = u4_bag_add(lane, fad, m->fan);
  pol = _mill_play(m, bir, fug);
  m->fan = fan;

  /* Test for trivial reap loop.  XX - needed?
  */
  {
    u4_noun p_pol, q_pol;

    if ( u4_b_pq(pol, u4_atom_post, &p_pol, &q_pol) &&
         u4_n_eq(p_pol, bir) &&
         u4_n_eq(q_pol, fug) )
    {
      return _mill_fail(m, "infinite recursion");
    }
  }
  return pol;
}
