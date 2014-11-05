/* j/5/lore.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3_cqe_lore(u3_atom lub)
  {
    c3_w    len_w = u3_cr_met(3, lub);
    c3_w    pos_w = 0;
    u3_noun tez = u3_nul;

    while ( 1 ) {
      c3_w meg_w;

      if ( pos_w >= len_w ) {
        return u3_ckb_flop(tez);
      } else {
        meg_w = 0;

	c3_y byt_y;
        while ( 1 ) {
          byt_y = u3_cr_byte(pos_w + meg_w, lub);

          if ( (10 == byt_y) || (0 == byt_y) ) {
            break;
          } else meg_w++;
        }

        if ((byt_y == 0) && ((pos_w + meg_w + 1) < len_w)) {
	  return u3_cm_bail(c3__exit);
	}

        {
          c3_y* byts_y = alloca(meg_w);

          u3_cr_bytes(pos_w, meg_w, byts_y, lub);
          tez = u3nc(u3_ci_bytes(meg_w, byts_y), tez);
          pos_w += (meg_w + 1);
        }
      }
    }
  }

  u3_noun
  u3_cwe_lore(u3_noun cor)
  {
    u3_noun lub;

    if ( (u3_none == (lub = u3_cr_at(u3_cv_sam, cor))) ||
         (c3n == u3ud(lub)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_lore(lub);
    }
  }
