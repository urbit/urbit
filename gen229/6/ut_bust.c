/* j/6/bust.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, bust)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_noun yam = j2_mcy(Pt6, ut, burn)(wir_r, van, sut);
    u2_noun yib = u2_bc
      (wir_r,
       u2_bc(wir_r, u2_rx(wir_r, u2_bi_h(wir_r, u2_bi_h(wir_r, yam))),
                    u2_rx(wir_r, dib)),
       u2_rx(wir_r, u2_bi_t(wir_r, yam)));
    u2_noun wox = j2_mcy(Pt6, ut, mint)(wir_r, van, sut, c3__noun, u2_blip);
    u2_noun ret = u2_nk_nock(wir_r, yib, u2_t(wox));

    if ( u2_none == ret ) {
      return u2_bl_error(wir_r, "bust-nock");
    }
    u2_rz(wir_r, wox);
    u2_rz(wir_r, yam);

    return ret;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, bust)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, bust)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &dib, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, bust)(wir_r, van, sut, dib);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, bust)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dib)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "bust");

    if ( u2_none == hoc ) {
      c3_assert(!"register bust");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, dib), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, bust)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, bust)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, bust)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, bust)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, bust)(wir_r, van, sut, dib);
      }
      else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, dib);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, bust)(wir_r, van, sut, dib);

          return u2_rl_save_cell(wir_r, fun_m, sut, dib, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, bust)(wir_r, van, sut, dib);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, bust)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &dib, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, dib));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, bust)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, bust), 
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, bust)
    },
    { }
  };
