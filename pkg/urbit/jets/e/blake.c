/* j/5/blake.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/

  static u3_atom
  _cqe_blake(u3_atom wid, u3_atom dat,
             u3_atom wik, u3_atom dak,
             u3_atom out)
  {
    c3_w wid_w;
    if ( !u3r_word_fit(&wid_w, wid) ) {
      // impossible to represent an atom this large
      return u3m_bail(c3__fail);
    }
    else {
      // the hoon adjusts these widths to its liking
      int err;
      u3_atom ret;
      c3_y  out_y[64], dak_y[64];
      c3_w  wik_w = c3_min(wik, 64),
            out_w = c3_max(1, c3_min(out, 64));
      c3_y *dat_y = u3r_bytes_alloc(0, wid_w, dat);

      u3r_bytes(0, wik_w, dak_y, dak);
      err = urcrypt_blake2(wid_w, dat_y, wik_w, dak_y, out_w, out_y);
      u3a_free(dat_y);

      if ( 0 == err ) {
        return u3i_bytes(out_w, out_y);
      }
      else {
        return u3_none;
      }
    }
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
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("blake", _cqe_blake(wid, dat, wik, dak, out));
    }
  }
