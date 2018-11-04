/* j/5/blake.c
**
*/
#include "all.h"

#include <argon2.h>
#include "../src/blake2/blake2.h"

#if 0
static void pretty_print_hex(char * headline,
                             c3_y * ptr,
                             c3_w num_bytes)
{
  fprintf(stderr, "\r%s:\n\r  ", headline);
  for (c3_ws ii=0 ; ii < num_bytes; ii++) {
    fprintf(stderr, "%02x", ptr[ii]);
    if (0 == (ii + 1) % 16){
          fprintf(stderr, "\n\r  ");
    } else {
          fprintf(stderr, ", ");
    }
  }
  fprintf(stderr, "\n\r");

}
#endif

u3_noun
u3qe_blake(u3_atom msg, u3_atom key, u3_atom out)
{
  c3_assert(_(u3a_is_cat(out)));

  // msg
  u3_noun men; // length
  u3_noun mod; // body

  u3r_mean(msg,
           2, &men,
           3, &mod,
           0);

  // meta length: length of the length
  c3_w met_w = u3r_met(3, men);

  if (met_w > 4)
  {
    fprintf(stderr, "\rblake jet: msg size size too big\n");
    return u3m_bail(c3__exit);
  }

  c3_w men_w;
  men_w = u3r_word(0, men);

  c3_y* mod_y = (c3_y*)u3a_malloc(men_w);
  u3r_bytes(0, men, (void*)mod_y, mod);

  // key
  u3_noun ken; // length
  u3_noun kod; // body

  u3r_mean(key,
           2, &ken,
           3, &kod,
           0);

  // meta length: length of the length
  c3_w mek_w = u3r_met(3, ken);

  if (mek_w > 4)
  {
    fprintf(stderr, "\rblake jet: key size size too big\n");
    return u3m_bail(c3__exit);
  }

  c3_w ken_w;
  ken_w =  u3r_word(0, ken);

  c3_y* kod_y = (c3_y*)u3a_malloc(ken_w);
  u3r_bytes(0, ken, (void*)kod_y, kod);

  int ret;
  c3_y out_y[64];
  ret = blake2b(out_y,  // OUT: output
                out,    // IN: max output size
                mod_y,  // IN: msg body
                men_w,  // IN: msg len
                kod_y,  // IN: key body
                ken_w); // IN: key len

  /* free() BEFORE checking error code;
     we don't want to leak memory if we return early
  */
  u3a_free(mod_y);
  u3a_free(kod_y);

  if (ret != 0)
  {
    fprintf(stderr, "\rblake jet: cryto lib error\n");
    return u3m_bail(c3__exit);
  }

  return(u3kc_rev(3, out, u3i_bytes(out, out_y)));
}



u3_noun
u3we_blake(u3_noun cor)
{
  u3_noun msg, byt, out;

  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2, &msg,
                        u3x_sam_6, &byt,
                        u3x_sam_7, &out,
                        0)) ||
       (c3n == u3du(msg)) ||
       (c3n == u3du(byt)) ||
       (c3n == u3ud(out)) )
  {
    fprintf(stderr, "\rblake jet: arguments error\n");
    return u3m_bail(c3__exit);
  } else {
    return u3qe_blake(msg, byt, out);
  }
}
