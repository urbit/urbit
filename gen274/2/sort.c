/* j/2/sort.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, sort)(u2_wire wir_r, 
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    //  must think about
    //
    return u2_none;
  }
  u2_noun                                                         // transfer
  j2_mb(Pit, sort)(u2_wire wir_r, 
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, 8, &a, 9, &b, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, sort)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, sort)[] = {
    { ".3", j2_mb(Pit, sort), u2_no, u2_none, u2_none },
    { }
  };

