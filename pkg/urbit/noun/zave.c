/* g/z.c
**
*/
#include "all.h"

/* u3z_key(): construct a memo cache-key.  Arguments retained.
*/
u3_noun
u3z_key(c3_m fun, u3_noun one)
{
  return u3nc(fun, u3k(one));
}
u3_noun
u3z_key_2(c3_m fun, u3_noun one, u3_noun two)
{
  return u3nt(fun, u3k(one), u3k(two));
}
u3_noun
u3z_key_3(c3_m fun, u3_noun one, u3_noun two, u3_noun tri)
{
  return u3nq(fun, u3k(one), u3k(two), u3k(tri));
}
u3_noun
u3z_key_4(c3_m fun, u3_noun one, u3_noun two, u3_noun tri, u3_noun qua)
{
  return u3nc(fun, u3nq(u3k(one), u3k(two), u3k(tri), u3k(qua)));
}

/* u3z_find(): find in memo cache.  Arguments retained.
*/
u3_weak
u3z_find(u3_noun key)
{
  return u3h_get(u3R->cax.har_p, key);
}
u3_weak
u3z_find_m(c3_m fun, u3_noun one)
{
  u3_noun key = u3nc(fun, u3k(one));
  u3_weak val;

  val = u3h_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}

/* u3z_save(): save in memo cache. TRANSFER key; RETAIN val
*/
u3_noun
u3z_save(u3_noun key, u3_noun val)
{
  u3h_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}

/* u3z_save_m(): save in memo cache. Arguments retained.
*/
u3_noun
u3z_save_m(c3_m fun, u3_noun one, u3_noun val)
{
  u3_noun key = u3nc(fun, u3k(one));

  u3h_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}

/* u3z_uniq(): uniquify with memo cache.
*/
u3_noun
u3z_uniq(u3_noun som)
{
  u3_noun key = u3nc(c3__uniq, u3k(som));
  u3_noun val = u3h_get(u3R->cax.har_p, key);

  if ( u3_none != val ) {
    u3z(key); u3z(som); return val;
  }
  else {
    u3h_put(u3R->cax.har_p, key, u3k(som));
    return som;
  }
}
