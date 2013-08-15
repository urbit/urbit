/* j/6/emit.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, emit)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_atom axe,                              //  retain
                        u2_noun mun,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_noun luf = u2_nul;

    mun = u2_rx(wir_r, mun);
    while ( 1 ) {
      if ( u2_nul == har ) {
        u2_noun pro = u2_bc(wir_r, mun, j2_mby(Pit, hike)(wir_r, axe, luf));

        u2_rl_lose(wir_r, luf);
        return pro;
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
          u2_noun taq = j2_mcy(Pit, ut, tuck)
            (wir_r, van, mun, hep, sut, qi_har);
          u2_noun p_taq = u2_h(taq);
          u2_noun q_taq = u2_t(taq);

          har = t_har;

          u2_rl_lose(wir_r, mun);
          mun = u2_rx(wir_r, p_taq);

          luf = u2_bc(wir_r, u2_rx(wir_r, q_taq), luf);

          u2_rl_lose(wir_r, hep);
          u2_rl_lose(wir_r, taq);
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, emit)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, axe, mun, har;

    if ( (u2_no == u2_mean(cor, 8, &axe, 18, &mun, 19, &har, 5, &van, 0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(4, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, ut, emit)(wir_r, van, sut, axe, mun, har);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, emit)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, emit), Tier6_b, u2_none, u2_none },
    { }
  };

