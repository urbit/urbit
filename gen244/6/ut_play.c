/* j/6/play.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, play)(u2_wire wir_r, 
                          u2_noun van,                            //  retain
                          u2_noun sut,                            //  retain
                          u2_noun gen)                            //  retain
  {
    // u2_noun von = u2_bn_cook(wir_r, van, "vet", u2_no);
    u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 
                                         j2_ut_van_fab, u2_no,
                                         0);
    u2_noun ret;
#if 0
    ret = _play_in(wir_r, von, sut, gen);
#else
    {
      u2_noun fug = j2_mcy(Pt6, ut, mint)(wir_r, von, sut, c3__noun, gen);

      ret = u2_rx(wir_r, u2_h(fug));
      u2_rz(wir_r, fug);
    }
#endif

    u2_rl_lose(wir_r, von);
    return ret;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, play)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, play)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
  {
    u2_noun sut, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &gen, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, play)(wir_r, van, sut, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, play)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "play");

    if ( u2_none == hoc ) {
      c3_assert(!"register play");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, gen), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, play)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, play)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, play)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, play)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, play)(wir_r, van, sut, gen);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, play)(wir_r, van, sut, gen);
      fol = u2_t(cor);

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
  j2_mcj(Pt6, ut, play)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, play), Tier6_b, u2_none, u2_none },
    { }
  };
