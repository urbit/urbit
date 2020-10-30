/* jets/d/in_wyt.c
**
*/
#include "all.h"

STATIC_ASSERT( (UINT32_MAX > u3a_cells),
               "width precision" );

static c3_w
_wyt_in(u3_noun a)
{
  if ( u3_nul == a ) {
    return 0;
  }
  else {
    u3_noun l_a, r_a;
    u3x_trel(a, 0, &l_a, &r_a);

    return 1 + _wyt_in(l_a) + _wyt_in(r_a);
  }
}

u3_noun
u3qdi_wyt(u3_noun a)
{
  return u3i_word(_wyt_in(a));
}

u3_noun
u3wdi_wyt(u3_noun cor)
{
  return u3qdi_wyt(u3x_at(u3x_con_2, cor));
}
