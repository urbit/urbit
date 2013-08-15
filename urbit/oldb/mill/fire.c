/* mill/fire.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_fire(): fire [tel hum] as (gate arg).
*/
u4_nock
_mill_fire(u4_milr m,
           u4_nock tel,
           u4_nock hum)
{
  u4_lane lane = m->lane;
  u4_nock arg  = u4_k_cell(lane, u4_noun_0, u4_noun_3);
  u4_nock env  = u4_k_cell(lane, u4_noun_0, u4_noun_9);
  u4_nock noc  = u4_k_cell(lane, u4_noun_0, u4_noun_5);

  return _mill_comp
    (m,
     _mill_cons(m, tel, hum),
     u4_k_trel
      (lane, 
       u4_noun_3,
       u4_k_cell(lane, u4_k_cell(lane, arg, env), noc),
       noc));
}
