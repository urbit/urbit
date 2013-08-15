/* mill/bark.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_bark(): compute [axis type form] for a bend.
*/
u4_noun
_mill_bark(u4_milr m,
           u4_type ter,
           u4_gene nuv)
{
  u4_form fol = _mill_make(m, ter, nuv);
  u4_axis lub;
  u4_type cav;
  {
    u4_noun foo, bar, moo;

    if ( u4_b_p(fol, u4_noun_0, &lub) ) {
      cav = _mill_play(m, ter, nuv);
    }
    else if ( u4_b_pq(fol, u4_noun_3, &foo, &bar) &&
              u4_b_p(foo, u4_noun_0, &moo) &&
              u4_b_p(bar, u4_noun_0, 0) )
    {
      lub = moo;
      cav = _mill_hack(m, lub, ter);
    }
    else {
      return _mill_fail(m, "bend malfunction");
    }
  }
  return u4_k_cell(m->lane, lub, cav);
}
