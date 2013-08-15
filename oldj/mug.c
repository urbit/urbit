/* j/mug.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_mug():
*/
u3_fox
u3_zx_mug(u3_z   z,
          u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));

  /* The mug is always <= 31 bits, ie, a cat.
  */
  return u3_lm_mug(z, sam);
}
