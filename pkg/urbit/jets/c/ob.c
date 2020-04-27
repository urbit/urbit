/* j/3/ob.c
**
*/
#include "all.h"

u3_noun
u3qc_ob_eff(u3_atom j, u3_atom r)
{
  c3_d found = 0;
  switch (j) {
    case 0:
      found = 0xb76d5eed;
      break;
    case 1:
      found = 0xee281300;
      break;
    case 2:
      found = 0x85bcae01;
      break;
    case 3:
      found = 0x4b387af7;
      break;
    default:
      // the internal snag would have failed.
      return u3m_bail(c3__exit);
  }

  return u3qc_muk(u3i_chubs(1, &found), 2, r);
}

u3_noun
u3wc_ob_eff(u3_noun cor)
{
  u3_noun j, r;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &j,
                             u3x_sam_3, &r, 0)) ||
       (c3n == u3ud(j)) ||
       (c3n == u3ud(r)) )
  {
    fprintf(stderr, "u3wc_ob_eff failed to unpack\r\n");
    return u3m_bail(c3__exit);
  }
  else {
    return u3qc_ob_eff(j, r);
  }
}

