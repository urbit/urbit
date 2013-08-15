/* mill/comp.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_comp(): compose a pipe.
*/
u4_form
_mill_comp(u4_milr m,
           u4_form mal,
           u4_form buz)
{
  u4_lane lane = m->lane;
  u4_noun p_mal, p_buz, q_buz, pp_buz, pq_buz;

  if ( u4_b_p(mal, u4_noun_0, &p_mal) ) {
    if ( u4_b_p(buz, u4_noun_0, &p_buz) ) {
      return u4_k_cell(lane, u4_noun_0, u4_op_peg(lane, p_mal, p_buz));
    }
    else if ( u4_b_pq(buz, u4_noun_3, &p_buz, &q_buz) &&
              u4_b_p(p_buz, u4_noun_0, &pp_buz) &&
              u4_b_p(q_buz, u4_noun_0, &pq_buz) )
    {
      return u4_k_trel
        (lane, u4_noun_3, 
               u4_k_cell(lane, u4_noun_0, u4_op_peg(lane, p_mal, pp_buz)),
               u4_k_cell(lane, u4_noun_0, u4_op_peg(lane, p_mal, pq_buz)));
    }
  }
  return u4_k_qual(lane, u4_noun_3, mal, u4_noun_1, buz);
}
