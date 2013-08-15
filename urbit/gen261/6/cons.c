/* j/6/cons.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, cons)(u2_wire wir_r, 
                    u2_noun vur,                                  //  retain
                    u2_noun sed)                                  //  retain
  {
    u2_noun p_vur, p_sed;

    if ( u2_yes == u2_as_p(vur, u2_nock_bone, &p_vur) && 
         u2_yes == u2_as_p(sed, u2_nock_bone, &p_sed) ) {
      return u2_bt(wir_r, u2_nock_bone, 
                          u2_bx(wir_r, p_vur),
                          u2_bx(wir_r, p_sed));
    }
    else if ( u2_yes == u2_as_p(vur, u2_nock_frag, &p_vur) && 
              u2_yes == u2_as_p(sed, u2_nock_frag, &p_sed) &&
              !(u2_yes == u2_sing(_1, p_vur)) &&
              !(u2_yes == u2_sing(p_vur, p_sed)) &&
              (_0 == u2_nord(p_vur, p_sed)) )
    {
      u2_atom fub = j2_mbc(Pt1, div)(wir_r, p_vur, _2);
      u2_atom nof = j2_mbc(Pt1, div)(wir_r, p_sed, _2);

      if ( u2_yes == u2_sing(fub, nof) ) {
        u2_rl_lose(wir_r, nof);

        return u2_bc(wir_r, u2_nock_frag, fub);
      }
      else {
        u2_rl_lose(wir_r, fub);
        u2_rl_lose(wir_r, nof);
      }
    }
    return u2_bc(wir_r, u2_bx(wir_r, vur), u2_bx(wir_r, sed));
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, cons)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun vur, sed;

    if ( u2_no == u2_mean(cor, 8, &vur, 9, &sed, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, cons)(wir_r, vur, sed);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, cons)[] = {
    { ".3", c3__hevy, j2_mb(Pt6, cons), Tier6_a, u2_none, u2_none },
    { }
  };
