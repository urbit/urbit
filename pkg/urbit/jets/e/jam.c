/* j/5/jam.c
**
*/
#include "all.h"

u3_noun
u3qe_jam(u3_atom a)
{
  c3_w  bit_w, *sal_w;
  c3_w* wor_w = u3s_jam_fib(a, &bit_w);
  c3_w  len_w = bit_w >> 5;
  if ( (len_w << 5) != bit_w ) {
    ++len_w;
  }
  sal_w = u3a_slab(len_w);
  memcpy(sal_w, wor_w, len_w*sizeof(c3_w));
  u3a_wfree(wor_w);
  return u3a_moot(sal_w);
}

u3_noun
u3we_jam(u3_noun cor)
{
  return u3qe_jam(u3x_at(u3x_sam, cor));
}

u3_atom
u3ke_jam(u3_noun a)
{
  u3_atom b = u3qe_jam(a);
  u3z(a);
  return b;
}
