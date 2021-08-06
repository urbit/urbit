/* j/5/ripe.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqe_ripe(u3_atom wid, u3_atom dat)
  {
    c3_w len_w;
    if ( !u3r_word_fit(&len_w, wid) ) {
      return u3m_bail(c3__fail);
    }
    else {
      u3_atom ret;
      c3_y out_y[20];
      c3_y *dat_y = u3r_bytes_alloc(0, len_w, dat);

      ret = ( 0 == urcrypt_ripemd160(dat_y, len_w, out_y) )
          ? u3i_bytes(20, out_y)
          : u3_none;

      u3a_free(dat_y);
      return ret;
    }
  }

  u3_noun
  u3we_ripe(u3_noun cor)
  {
    u3_noun wid, dat;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &wid,
                               u3x_sam_3, &dat, 0) ||
                 u3ud(wid) || u3ud(dat))
       )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3l_punt("ripe", _cqe_ripe(wid, dat));
    }
  }
