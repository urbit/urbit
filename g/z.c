/* g/z.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_cz_find(): find in memo cache.  Arguments retained.
*/
u3_weak 
u3_cz_find(u3_mote fun, u3_noun one)
{
  u3_noun key = u3nc(fun, u3k(one));
  u3_noun val;

  val = u3_ch_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak 
u3_cz_find_2(u3_mote fun, u3_noun one, u3_noun two)
{
  u3_noun key = u3nt(fun, u3k(one), u3k(two));
  u3_noun val;

  val = u3_ch_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak 
u3_cz_find_3(u3_mote fun, u3_noun one, u3_noun two, u3_noun tri)
{
  u3_noun key = u3nq(fun, u3k(one), u3k(two), u3k(tri));
  u3_noun val;

  val = u3_ch_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak 
u3_cz_find_4(u3_mote fun, u3_noun one, u3_noun two, u3_noun tri, u3_noun qua)
{
  u3_noun key = u3nc(fun, u3nq(u3k(one), u3k(two), u3k(tri), u3k(qua)));
  u3_noun val;

  val = u3_ch_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}

/* u3_cz_save*(): save in memo cache.
*/
u3_noun 
u3_cz_save(u3_mote fun, u3_noun one, u3_noun val)
{
  u3_noun key = u3nc(fun, u3k(one));

  u3_ch_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun 
u3_cz_save_2(u3_mote fun, u3_noun one, u3_noun two, u3_noun val)
{
  u3_noun key = u3nt(fun, u3k(one), u3k(two));

  u3_ch_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun 
u3_cz_save_3(u3_mote fun, u3_noun one, u3_noun two, u3_noun tri, u3_noun val)
{
  u3_noun key = u3nq(fun, u3k(one), u3k(two), u3k(tri));

  u3_ch_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun 
u3_cz_save_4(u3_mote fun, 
             u3_noun one, 
             u3_noun two, 
             u3_noun tri, 
             u3_noun qua, 
             u3_noun val)
{
  u3_noun key = u3nc(fun, u3nq(u3k(one), u3k(two), u3k(tri), u3k(qua)));

  u3_ch_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}

/* u3_cz_uniq(): uniquify with memo cache.
*/
u3_noun 
u3_cz_uniq(u3_noun som)
{
  u3_noun key = u3nc(c3__uniq, u3k(som));
  u3_noun val = u3_ch_get(u3R->cax.har_p, key);

  if ( u3_none != val ) {
    u3z(key); u3z(som); return val;
  } 
  else {
    u3_ch_put(u3R->cax.har_p, key, u3k(som));
    return som;
  }
}
