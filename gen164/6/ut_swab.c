/* j/6/swab.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/

/* functions
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, snub)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    if ( u2_no == u2_dust(har) ) {
      return u2_nul;
    } else {
      u2_noun i_har = u2_h(har);
      u2_noun t_har = u2_t(har);
      u2_noun pi_har = u2_h(i_har);
      u2_noun qi_har = u2_t(i_har);
      u2_noun peh = j2_mbc(Pt2, flop)(wir_r, pi_har);
      u2_noun ret = u2_bc(wir_r, u2_bc(wir_r, peh, u2k(qi_har)),
                                 j2_mcx(Pt6, ut, snub)
                                    (wir_r, van, sut, t_har));

      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, snub)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, snub)(u2_wire wir_r,
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, har;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &har,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, snub)(wir_r, van, sut, har);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, snub)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "snub");

    if ( u2_none == hoc ) {
      c3_assert(!"register snub");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, har),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, snub)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, snub)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, snub)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, snub)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, snub)(wir_r, van, sut, har);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, snub)(wir_r, van, sut, har);
      fol = u2_h(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, snub)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, snub), Tier6_c, u2_none, u2_none },
    { }
  };
