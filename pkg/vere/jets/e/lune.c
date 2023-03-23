/* j/5/lune.c
**
*/
#include "all.h"


  u3_noun
  u3qe_lune(u3_atom lub)
  {
    if (lub == 0) {
      return u3_nul;
    }

    {
      c3_w end_w  = u3r_met(3, lub) - 1;
      c3_w pos_w  = end_w;
      u3_noun lin = u3_nul;

      if (u3r_byte(pos_w, lub) != 10) {
        return u3m_error("noeol");
      }

      if (pos_w == 0) {
        return u3nc(u3_nul, lin);
      }

      while (--pos_w) {
        if (u3r_byte(pos_w, lub) == 10) {
          lin = u3nc(u3qc_cut(3, (pos_w + 1), (end_w - pos_w - 1), lub), lin);
          end_w = pos_w;
        }
      }

      if (u3r_byte(pos_w, lub) == 10) {
        return u3nc(u3_nul,
                    u3nc(u3qc_cut(3, (pos_w + 1), (end_w - pos_w - 1), lub), lin));
      }

      return u3nc(u3qc_cut(3, pos_w, (end_w - pos_w), lub), lin);
    }
  }

  u3_noun
  u3we_lune(u3_noun cor)
  {
    u3_noun lub;

    if ( (u3_none == (lub = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(lub)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_lune(lub);
    }
  }
