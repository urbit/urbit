/* mill/repo.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_repo(): replay load.
*/
u4_mold
_mill_repo(u4_milr m,
           u4_mold tip,
           u4_gene gen)
{
  u4_lane lane = m->lane;
  u4_noun fad = u4_k_cell(lane, tip, gen);
  u4_noun fan;
  u4_mold pol;
  u4_nopt zax;

  zax = u4_tab_get(fad, m->rep);
  if ( u4_bull != zax ) {
    return zax;
  }

  if ( u4_bag_in(fad, m->fan) ) {
    // printf("inference recursion\n");
    // abort();

    return _mill_fail(m, "repo: inference recursion");
  }

  fan = m->fan;
  m->fan = u4_bag_add(lane, fad, m->fan);
  pol = _mill_play(m, gen, tip);
  m->fan = fan;

  m->rep = u4_tab_add(lane, fad, pol, m->rep);
  return pol;
}
