/* j/4/by_run.c
**
*/
#include "all.h"

//  [a] is RETAINED, [set] is TRANSFERRED
//
static u3_noun
_by_key(u3_noun a, u3_noun set)
{
  if ( u3_nul == a ) {
    return set;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    {
      u3_noun new = u3qdi_put(set, u3h(n_a));
      u3z(set);
      set = new;
    }

    set = _by_key(l_a, set);

    return _by_key(r_a, set);
  }
}

u3_noun
u3qdb_key(u3_noun a)
{
  return _by_key(a, u3_nul);
}

u3_noun
u3wdb_key(u3_noun cor)
{
  return u3qdb_key(u3x_at(u3x_con_sam, cor));
}
