/* z/c.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zc_trap():
**
**   Trap with longjmp().  Does not return.
*/
int
u3_zc_trap(u3_z            z,
           enum u3_zc_trap trap_a)
{
  printf("trap: %d\n", trap_a);
  longjmp(z->jmp_lum, trap_a);

  return 0;
}

/* u3_zc_alloc_w(): 
**
**   Allocate (w_a) words on (z).  Traps if allocation fails.
*/
void *
u3_zc_alloc_w(u3_z z,
              u3_w w_a)
{
  void *v_fup = u3_lm_alloc(z, w_a);

  if ( !v_fup ) {
    u3_zc_trap(z, u3_zc_trap_fail);
    return 0;
  }
  else return v_fup;
}

/* u3_zc_bytes():
**
**   Copy (w_a) bytes from (y_b) into an atom on the hat of (l).
*/
u3_fox
u3_zc_bytes(u3_z z,
            u3_w w_a,
            u3_y *y_b)
{
  u3_rat tav = u3_ln_bytes(z, w_a, y_b);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_string():
**
**   u3_zc_bytes(l, strlen(c_a), (u3_y *)c_a);
*/
u3_fox
u3_zc_string(u3_z z,
             u3_c *c_a)
{
  u3_rat tav = u3_ln_string(z, c_a);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_cell(): 
**
**   Produce the cell (a b) on the hat of (l), or u3_l_none if
**   this would overflow the clam.
*/
u3_fox
u3_zc_cell(u3_z   z,
           u3_fox a,
           u3_fox b)
{
  u3_rat tav = u3_ln_cell(z, a, b);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_ice():
**
**   Produce a noun equivalent to (a), which does not reference
**   any data on the can of (l).
*/
u3_fox
u3_zc_ice(u3_z   z,
          u3_fox a)
{
  u3_rat tav = u3_ln_ice(z, a);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_mp():
**
**   Copy the GMP integer (mp_a) into an atom on the hat of (l).
*/
u3_fox
u3_zc_mp(u3_z  z,
         mpz_t mp_a)
{
  u3_rat tav = u3_ln_mp(z, mp_a);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_trel(): 
**
**   Produce the trel [a b c] on the hat of [l], or u3_l_none if
**   this would overflow the clam.
*/
u3_fox
u3_zc_trel(u3_z   z,
           u3_fox a,
           u3_fox b,
           u3_fox c)
{
  u3_rat tav = u3_ln_trel(z, a, b, c);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}

/* u3_zc_words():
**
**   Copy (w_a) words from (w_b) into an atom on the hat of (l).
*/
u3_fox
u3_zc_words(u3_z  z,
            u3_w  w_a,
            u3_w  *w_b)
{
  u3_rat tav = u3_ln_words(z, w_a, w_b);

  if ( u3_none == tav ) {
    return u3_zc_trap(z, u3_zc_trap_fail);
  }
  else return tav;
}
