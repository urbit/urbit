/* j/5/lore.c
**
*/
#include "all.h"


  u3_noun
  u3qe_lore(u3_atom lub)
  {
    c3_w    len_w = u3r_met(3, lub);
    c3_w    pos_w = 0;
    u3_noun tez = u3_nul;

    while ( 1 ) {
      c3_w meg_w = 0;
      c3_o end_y;

      c3_y byt_y;
      while ( 1 ) {
        if ( (pos_w + meg_w) >= len_w ) {
          byt_y = 0;
          end_y = c3y;
          break;
        }
        byt_y = u3r_byte(pos_w + meg_w, lub);

        if ( 10 == byt_y ) {
          end_y = c3n;
          break;
        } else meg_w++;
      }

      {
        c3_y* byts_y = 0;

        if ( 0 != meg_w ) {
          byts_y = alloca(meg_w);
          u3r_bytes(pos_w, meg_w, byts_y, lub);
        }

        if ( _(end_y) ) {
          return u3kb_flop(u3nc(u3i_bytes(meg_w, byts_y), tez));
        }
        if ( pos_w >= len_w ) {
          return u3kb_flop(tez);
        }
        tez = u3nc(u3i_bytes(meg_w, byts_y), tez);
        pos_w += (meg_w + 1);
      }
    }
  }

  u3_noun
  u3we_lore(u3_noun cor)
  {
    u3_noun lub;

    if ( (u3_none == (lub = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(lub)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_lore(lub);
    }
  }
