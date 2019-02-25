/* g/z.c
**
*/
#include "all.h"

/* u3z_find(): find in memo cache.  Arguments retained.
*/
u3_weak
u3z_find(c3_m fun, u3_noun one)
{
  u3_noun key = u3nc(fun, u3k(one));
  u3_noun val;

  val = u3h_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak
u3z_find_2(c3_m fun, u3_noun one, u3_noun two)
{
  u3_noun key = u3nt(fun, u3k(one), u3k(two));
  u3_noun val;

  val = u3h_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak
u3z_find_3(c3_m fun, u3_noun one, u3_noun two, u3_noun tri)
{
  u3_noun key = u3nq(fun, u3k(one), u3k(two), u3k(tri));
  u3_noun val;

  val = u3h_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}
u3_weak
u3z_find_4(c3_m fun, u3_noun one, u3_noun two, u3_noun tri, u3_noun qua)
{
  u3_noun key = u3nc(fun, u3nq(u3k(one), u3k(two), u3k(tri), u3k(qua)));
  u3_noun val;

  val = u3h_get(u3R->cax.har_p, key);
  u3z(key);
  return val;
}

/* u3z_save*(): save in memo cache.
*/
u3_noun
u3z_save(c3_m fun, u3_noun one, u3_noun val)
{
  u3_noun key = u3nc(fun, u3k(one));

  u3h_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun
u3z_save_2(c3_m fun, u3_noun one, u3_noun two, u3_noun val)
{
  u3_noun key = u3nt(fun, u3k(one), u3k(two));

  u3h_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun
u3z_save_3(c3_m fun, u3_noun one, u3_noun two, u3_noun tri, u3_noun val)
{
  u3_noun key = u3nq(fun, u3k(one), u3k(two), u3k(tri));

  u3h_put(u3R->cax.har_p, key, u3k(val));
  u3z(key);
  return val;
}
u3_noun
u3z_save_4(c3_m fun,
             u3_noun one,
             u3_noun two,
             u3_noun tri,
             u3_noun qua,
             u3_noun val)
{
  u3_noun key = u3nc(fun, u3nq(u3k(one), u3k(two), u3k(tri), u3k(qua)));

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
