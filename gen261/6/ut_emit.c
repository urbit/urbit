/* j/6/emit.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _emit_each(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun men,
             u2_noun pi_har,
             u2_noun qi_har)
  {
    u2_noun hep = j2_mcy(Pt6, ap, rake)(wir_r, pi_har);
    u2_noun peh = j2_mbc(Pt2, flop)(wir_r, hep);
    u2_noun ret;
    
    if ( u2_nul == men ) {
      ret = u2_bc(wir_r, u2_nul, u2_nul);
    } 
    else {
      u2_noun i_men  = u2_h(men);
      u2_noun pi_men = u2_h(i_men);
      u2_noun qi_men = u2_t(i_men);
      u2_noun t_men  = u2_t(men);
      u2_noun lax    = _emit_each(wir_r, van, sut, t_men, pi_har, qi_har);
      u2_noun p_lax  = u2_h(lax);
      u2_noun q_lax  = u2_t(lax);
      u2_noun taq    = j2_mcy(Pt6, ut, tuck)
                        (wir_r, van, pi_men, peh, sut, qi_har);
      u2_noun p_taq  = u2_h(taq);
      u2_noun q_taq  = u2_t(taq);
      u2_noun qux;

      if ( u2_nul == p_lax ) {
        qux = u2_bc(wir_r, u2_nul, u2_rx(wir_r, q_taq));
      } else {
        if ( u2_no == u2_sing(q_taq, u2_t(p_lax)) ) {
          return u2_bl_error(wir_r, "emit-mate");
        }
        qux = u2_rx(wir_r, p_lax);
      }
      ret = u2_bc
        (wir_r, qux, 
                u2_bc(wir_r, 
                      u2_bc(wir_r, u2_rx(wir_r, p_taq), u2_rx(wir_r, qi_men)),
                      u2_rx(wir_r, q_lax)));

      u2_rz(wir_r, lax);
      u2_rz(wir_r, taq);
    }
    u2_rz(wir_r, hep);
    u2_rz(wir_r, peh);

    return ret;
  }
             
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, emit)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_atom axe,                              //  retain
                        u2_noun men,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_noun luf = u2_nul;

    men = u2_rx(wir_r, men);
    while ( 1 ) {
      if ( u2_nul == har ) {
        u2_noun ret = u2_bc(wir_r, men, j2_mby(Pt6, hike)(wir_r, axe, luf));

        u2_rl_lose(wir_r, luf);
        return ret;
      }
      else {
        u2_noun i_har = u2_h(har);
        u2_noun t_har = u2_t(har);

        if ( u2_no == u2_dust(i_har) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } 
        else {
          u2_noun pi_har = u2_h(i_har);
          u2_noun qi_har = u2_t(i_har);
          u2_noun ech    = _emit_each(wir_r, van, sut, men, pi_har, qi_har);
          u2_noun p_ech  = u2_h(ech);
          u2_noun q_ech  = u2_t(ech);

          har = t_har;

          u2_rl_lose(wir_r, men);
          men = u2_rx(wir_r, q_ech);

          luf = u2_bc(wir_r, u2_rx(wir_r, u2_t(p_ech)), luf);
          u2_rz(wir_r, ech);
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, emit)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, axe, mun, har;

    if ( (u2_no == u2_mean(cor, 8, &axe, 18, &mun, 19, &har, 5, &van, 0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(4, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, emit)(wir_r, van, sut, axe, mun, har);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, emit)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, emit), Tier6_b, u2_none, u2_none },
    { }
  };

