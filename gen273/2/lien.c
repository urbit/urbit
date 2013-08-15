/* j/2/lien.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, lien)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( _0 == a ) {
      return u2_no;
    } else {
      u2_weak loz;

      if ( u2_no == u2_dust(a) ) {
        return u2_none;
      }
      else switch ( (loz = u2_nk_mung(wir_r, b, u2_rx(wir_r, u2_h(a)))) ) {
        case u2_yes:  return u2_yes;
        case u2_no:   return j2_mbc(Pit, lien)(wir_r, u2_t(a), b);
        default:      u2_rl_lose(wir_r, loz); 
                      return u2_none;
      }
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pit, lien)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, 8, &a, 9, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, lien)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, lien)[] = {
    { ".3", j2_mb(Pit, lien), u2_no, u2_none, u2_none },
    { }
  };


