/* j/read.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_read_c():
*/
u3_fox
u3_zx_read_c(u3_z   z,
             u3_fox a)
{
  u3_rat rat = u3_b_read(&z->l, a);

  if ( u3_none == rat ) {
    return u3_zc_tank(z, c3__exit);
  }
  else return rat; 
}

/* u3_zx_read():
*/
u3_fox
u3_zx_read(u3_z   z,
           u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));

  return u3_zx_read_c(z, sam);
}
