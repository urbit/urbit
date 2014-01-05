/* j/6/sift.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, sift)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, sift)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &ref, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, sift)(wir_r, van, sut, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, sift)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "sift");

    if ( u2_none == hoc ) {
      c3_assert(!"register sift");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, ref), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, sift)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        //  c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, sift)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, sift)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
#if 0
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, sift)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, sift)(wir_r, van, sut, ref);
      }
      else {
        c3_m    fun_m = c3__sift;
        u2_noun pro   = u2_rl_find_trel(wir_r, fun_m, sut, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, sift)(wir_r, van, sut, ref);

          return u2_rl_save_trel(wir_r, fun_m, sut, ref, pro);
        }
      }
      return pro;
    } else 
#else
    {
      u2_noun cor, pro;

      cor = j2_mci(Pt6, ut, sift)(wir_r, van, sut, ref);

      pro = u2_nk_soft(wir_r, u2k(cor), u2h(cor));
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);

      return pro;
#endif
    }
  }

  u2_weak
  j2_mck(Pt6, ut, sift)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &ref, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc
        (wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, ref));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, sift)[] = {
    { ".2", c3__hevy, 
        j2_mc(Pt6, ut, sift), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, sift), c3__sift
    },
    { }
  };

