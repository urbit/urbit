/* j/5/jam.c
**
*/
#include "all.h"

u3_noun
u3qe_jam(u3_atom a)
{
#if 0
  if (c3y == u3du(a) && 1337 == u3h(a)) {
    c3_w siz_w, tot_w = 0;
    u3_noun som;
    for ( som = u3t(a); c3y == u3du(som); som = u3t(som) ) {
      siz_w = u3a_count_noun(u3h(som));
      tot_w += siz_w;
      if ( 0 == siz_w ) {
        u3l_log("item: B/0\r\n");
      }
      else {
        u3a_print_memory(stderr, "item", siz_w);
      }
    }
    if ( u3_blip != som ) {
      u3l_log("forgot to terminate list!\r\n");
    }
    c3_w mem_w = u3h_count(u3R->cax.har_p);

    for ( som = u3t(a); c3y == u3du(som); som = u3t(som) ) u3a_discount_noun(u3h(som));
    u3h_discount(u3R->cax.har_p);

    u3a_print_memory(stderr, "total", tot_w);
    u3a_print_memory(stderr, "memoization cache", mem_w);
    u3h_root* har_u = u3to(u3h_root, u3R->cax.har_p);
    u3l_log("memoization entries: %d\r\n", har_u->use_w);
		u3a_print_memory(stderr, "unused free", u3a_open(u3R));
    return tot_w;
  }
#endif

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
