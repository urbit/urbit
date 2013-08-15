/* j/add.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_add_c():
*/
u3_fox
u3_zx_add_c(u3_z   z,
            u3_fox a,
            u3_fox b)
{
  mpz_t mp_a, mp_b;

  u3_lr_mp(z, mp_a, a);
  u3_lr_mp(z, mp_b, b);

  mpz_add(mp_a, mp_a, mp_b); 
  mpz_clear(mp_b);

  return u3_zc_mp(z, mp_a);
}

/* u3_zx_add():
*/
u3_fox
u3_zx_add(u3_z   z,
          u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));
  u3_fox a, b;

  if ( (u3_no == u3_lr_cell(z, sam, &a, &b)) ||
       (u3_no == u3_lr_stud(z, a)) ||
       (u3_no == u3_lr_stud(z, b)) )
  {
    return u3_zc_tank(z, c3__punt);
  }
  else {
    return u3_zx_add_c(z, a, b);
  }
}
