/* j/1/dec.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, dec)(u2_wire wir_r, 
                   u2_atom a)                                     //  retain
  {
    if ( _0 == a ) {
      return u2_none;
    } 
    else {
      mpz_t a_mp;

      u2_mp(a_mp, a);
      mpz_sub_ui(a_mp, a_mp, 1);

      return u2_rl_mp(wir_r, a_mp);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, dec)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(4, cor))) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_none;
    } else {
      return j2_mbc(Pit, dec)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, dec)[] = { 
    { ".3", c3__lite, j2_mb(Pit, dec), Tier1, u2_none, u2_none },
    { }
  };
