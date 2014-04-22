/* j/6/ut_conk.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_bean                                                         //  transfer
  j2_mcx(Pt6, ut, conk)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun got)                              //  retain
  {
    if ( u2_yes == u2ud(got) ) {
      return j2_mby(Pt6, face)(wir_r, got, sut);
    }
    else switch ( u2h(got) ) {
      default: return u2_cm_bail(c3__fail);

      case 0: {
        return u2k(sut);
      }
      case 1: {
        return j2_mby(Pt6, face)
          (wir_r, u2h(u2t(got)),
                 j2_mcx(Pt6, ut, conk)(wir_r, van, sut, u2t(u2t(got))));
      }
      case 2: {
        u2_bean vet = u2_frag(j2_ut_van_vet, van);
        u2_noun hed, tal, ret;

        if ( u2_yes == vet ) {
          u2_noun cel = u2nt(c3__cell, c3__noun, c3__noun);

          if ( u2_no == j2_mcy(Pt6, ut, nest)(wir_r, van, cel, u2_yes, sut) ) {
            return u2_bl_bail(wir_r, c3__fail);
          }
          u2z(cel);
        }
        hed = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__both, 2);
        tal = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__both, 3);

        ret = j2_mby(Pt6, cell)
          (wir_r,
           j2_mcx(Pt6, ut, conk)(wir_r, van, hed, u2h(u2t(got))),
           j2_mcx(Pt6, ut, conk)(wir_r, van, tal, u2t(u2t(got))));

        u2z(hed);
        u2z(tal);

        return ret;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, conk)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, conk)(u2_wire wir_r,
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, got, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &got,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, conk)(wir_r, van, sut, got);
    }
  }

  u2_bean
  j2_mci(Pt6, ut, conk)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun got)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "conk");

    if ( u2_none == hoc ) {
      c3_assert(!"register conk");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat,
                                      u2_cv_sam, u2_rx(wir_r, got),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, conk)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, conk)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, conk)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun got)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, conk)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, conk)(wir_r, van, sut, got);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, conk)(wir_r, van, sut, got);
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
  j2_mcj(Pt6, ut, conk)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, conk), Tier6_b, u2_none, u2_none },
    { }
  };

