/* j/6/edit.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, edit)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun mun,                              //  retain
                        u2_noun har)                              //  retain
  {
    mun = u2_rx(wir_r, mun);
    while ( 1 ) {
      if ( u2_nul == har ) {
        return mun;
      }
      else {
        u2_noun i_har = u2_h(har);
        u2_noun t_har = u2_t(har);

        if ( u2_no == u2_dust(i_har) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun pi_har = u2_h(i_har);
          u2_noun qi_har = u2_t(i_har);
          u2_noun hep = j2_mcy(Pit, ap, rake)(wir_r, pi_har);
          u2_noun taq = j2_mcy(Pit, ut, tusk)
            (wir_r, van, mun, hep, sut, qi_har);

          har = t_har;

          u2_rl_lose(wir_r, hep);
          u2_rl_lose(wir_r, mun);
          mun = taq;
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, edit)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, mun, har;

    if ( (u2_no == u2_mean(cor, 8, &mun, 9, &har, 5, &van, 0)) ||
         (u2_none == (sut = u2_frag(4, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, ut, edit)(wir_r, van, sut, mun, har);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, edit)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, edit), Tier6_b, u2_none, u2_none },
    { }
  };
