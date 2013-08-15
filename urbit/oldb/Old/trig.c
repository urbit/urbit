/* mill/trig.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_trig(): triage by axis.
*/
void
_mill_trig(u4_lane lane,
           u4_log  lor,
           u4_log *zif,
           u4_log *gam, 
           u4_log *nog)
{
  if ( !u4_n_zero(lor) ) {
    u4_noun i_lor = u4_ch(lor);
    u4_noun t_lor = u4_ct(lor);
    u4_type pi_lor = u4_ch(i_lor);
    u4_type qi_lor = u4_ct(i_lor);

    _mill_trig(lane, t_lor, zif, gam, nog);

    if ( u4_n_eq(u4_noun_1, pi_lor) ) {
      *zif = u4_k_cell(lane, qi_lor, *zif);
    }
    else {
      if ( u4_n_eq(u4_noun_2, u4_op_tip(pi_lor)) ) {
        *gam = u4_k_cell
          (lane, u4_k_cell(lane, u4_op_tap(lane, pi_lor), qi_lor), *gam);
      } else {
        *nog = u4_k_cell
          (lane, u4_k_cell(lane, u4_op_tap(lane, pi_lor), qi_lor), *nog);
      }
    }
  }
}

