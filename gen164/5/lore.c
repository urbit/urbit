/* j/5/lore.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_noun                                                         //  produce
  j2_mbc(Pt5, lore)(u2_wire wir_r,
                    u2_atom lub)                                  //  retain
  {
    c3_w    len_w = u2_cr_met(3, lub);
    c3_w    pos_w = 0;
    u2_noun tez = u2_nul;

    while ( 1 ) {
      c3_w meg_w;

      if ( pos_w >= len_w ) {
        return u2_ckb_flop(tez);
      } else {
        meg_w = 0;

        while ( 1 ) {
          c3_y byt_y = u2_byte(pos_w + meg_w, lub);

          if ( (10 == byt_y) || (0 == byt_y) ) {
            break;
          } else meg_w++;
        }

        {
          c3_y* byt_y = alloca(meg_w);

          u2_bytes(pos_w, meg_w, byt_y, lub);
          tez = u2nc(u2_ci_bytes(meg_w, byt_y), tez);
          pos_w += (meg_w + 1);
        }
      }
    }
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, lore)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun lub;

    if ( (u2_none == (lub = u2_frag(u2_cv_sam, cor))) ||
         (u2_no == u2ud(lub)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mbc(Pt5, lore)(wir_r, lub);
    }
  }


/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, lore)[] = {
    { ".2", c3__lite, j2_mb(Pt5, lore), Tier5, u2_none, u2_none },
    { }
  };
