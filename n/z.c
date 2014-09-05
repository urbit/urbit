/* n/z.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_cz_find(): find in memo cache.  Arguments retained.
*/
u2_weak 
u2_cz_find(u2_mote fun, u2_noun one)
{
  u2_noun key = u2nc(fun, u2k(one));
  u2_noun val;

  val = u2_ch_get(u2R->cax.har_u, key);
  u2z(key);
  return val;
}
u2_weak 
u2_cz_find_2(u2_mote fun, u2_noun one, u2_noun two)
{
  u2_noun key = u2nt(fun, u2k(one), u2k(two));
  u2_noun val;

  val = u2_ch_get(u2R->cax.har_u, key);
  u2z(key);
  return val;
}
u2_weak 
u2_cz_find_3(u2_mote fun, u2_noun one, u2_noun two, u2_noun tri)
{
  u2_noun key = u2nq(fun, u2k(one), u2k(two), u2k(tri));
  u2_noun val;

  val = u2_ch_get(u2R->cax.har_u, key);
  u2z(key);
  return val;
}
u2_weak 
u2_cz_find_4(u2_mote fun, u2_noun one, u2_noun two, u2_noun tri, u2_noun qua)
{
  u2_noun key = u2nc(fun, u2nq(u2k(one), u2k(two), u2k(tri), u2k(qua)));
  u2_noun val;

  val = u2_ch_get(u2R->cax.har_u, key);
  u2z(key);
  return val;
}

/* u2_cz_save*(): save in memo cache.
*/
u2_noun 
u2_cz_save(u2_mote fun, u2_noun one, u2_noun val)
{
  u2_noun key = u2nc(fun, u2k(one));

  u2_ch_put(u2R->cax.har_u, key, u2k(val));
  u2z(key);
  return val;
}
u2_noun 
u2_cz_save_2(u2_mote fun, u2_noun one, u2_noun two, u2_noun val)
{
  u2_noun key = u2nt(fun, u2k(one), u2k(two));

  u2_ch_put(u2R->cax.har_u, key, u2k(val));
  u2z(key);
  return val;
}
u2_noun 
u2_cz_save_3(u2_mote fun, u2_noun one, u2_noun two, u2_noun tri, u2_noun val)
{
  u2_noun key = u2nq(fun, u2k(one), u2k(two), u2k(tri));

  u2_ch_put(u2R->cax.har_u, key, u2k(val));
  u2z(key);
  return val;
}
u2_noun 
u2_cz_save_4(u2_mote fun, 
             u2_noun one, 
             u2_noun two, 
             u2_noun tri, 
             u2_noun qua, 
             u2_noun val)
{
  u2_noun key = u2nc(fun, u2nq(u2k(one), u2k(two), u2k(tri), u2k(qua)));

  u2_ch_put(u2R->cax.har_u, key, u2k(val));
  u2z(key);
  return val;
}

/* u2_cz_uniq(): uniquify with memo cache.
*/
u2_noun 
u2_cz_uniq(u2_noun som)
{
  u2_noun key = u2nc(c3__uniq, u2k(som));
  u2_noun val = u2_ch_get(u2R->cax.har_u, key);

  if ( u2_none != val ) {
    u2z(key); u2z(som); return val;
  } 
  else {
    u2_ch_put(u2R->cax.har_u, key, u2k(som));
    return som;
  }
}
