/* j/5/blake.c
**
*/
#include "all.h"

#include <argon2.h>
#include <blake2.h>

/* functions
*/

  u3_noun
  u3qe_blake(u3_atom wid, u3_atom dat,
             u3_atom wik, u3_atom dak,
             u3_atom out)
  {
    c3_assert(_(u3a_is_cat(wid)) && _(u3a_is_cat(wik)) && _(u3a_is_cat(out)));

    // flip endianness for the internal blake2b function
    dat = u3qc_rev(3, wid, dat);
    dak = u3qc_rev(3, wik, dak);

    c3_y* dat_y = (c3_y*)u3a_malloc(wid);
    u3r_bytes(0, wid, (void*)dat_y, dat);

    c3_y* dak_y = (c3_y*)u3a_malloc(wik);
    u3r_bytes(0, wik, (void*)dak_y, dak);

    int ret;
    c3_y out_y[64];
    ret = blake2b(out_y,  // OUT: output
                  out,    // IN: max output size
                  dat_y,  // IN: msg body
                  wid,    // IN: msg len
                  dak_y,  // IN: key body
                  wik);   // IN: key len

    /* free() BEFORE checking error code;
       we don't want to leak memory if we return early
    */
    u3a_free(dat_y);
    u3a_free(dak_y);

    if ( 0 != ret )
    {
      fprintf(stderr, "\rblake jet: cryto lib error\n");
      return u3m_bail(c3__exit);
    }

    return u3kc_rev(3, out, u3i_bytes(out, out_y));
  }

  u3_noun
  u3we_blake(u3_noun cor)
  {
    u3_noun msg, key, out, // arguments
            wid, dat,      // destructured msg
            wik, dak;      // destructured key

    if ( c3n == u3r_mean(cor, u3x_sam_2, &msg,
                              u3x_sam_6, &key,
                              u3x_sam_7, &out, 0) ||
                u3r_cell(msg, &wid, &dat) || u3ud(wid) || u3ud(dat) ||
                u3r_cell(key, &wik, &dak) || u3ud(wik) || u3ud(dak) ||
                u3ud(out) )
    {
      fprintf(stderr, "\rblake jet: arguments error\n");
      return u3m_bail(c3__exit);
    } else {
      return u3qe_blake(wid, dat, wik, dak, out);
    }
  }
