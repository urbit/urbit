/* j/2/lent.c
**
*/
#include "all.h"

u3_noun
u3qb_lent(u3_noun a)
{
  if ( u3_nul == a ) {
    return 0;
  }

  //  loop until we overflow [len_w]
  //
  {
    c3_w len_w = 1;

    do {
      a = u3t(a);

      if ( u3_nul == a ) {
        return u3i_words(1, &len_w);
      }
    }
    while ( ++len_w );
  }

  //  continue with arbitrary precision
  //
  {
    u3_noun len = u3qc_bex(32);

    while ( 1 ) {
      a = u3t(a);

      if ( u3_nul == a ) {
        return len;
      }
      else {
        len = u3i_vint(len);
      }
    }
  }
}

u3_noun
u3wb_lent(u3_noun cor)
{
  return u3qb_lent(u3x_at(u3x_sam, cor));
}

u3_noun
u3kb_lent(u3_noun a)
{
  u3_noun b = u3qb_lent(a);
  u3z(a);
  return b;
}
