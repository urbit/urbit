/* j/6/bust.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, bust)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_noun yam = j2_mcy(Pt6, ut, burn)(van, sut);
    u2_noun yib = u2nc
      (
       u2nc(u2k(u2h(u2h(yam))),
                    u2k(dib)),
       u2k(u2t(yam)));
    u2_noun woo = u2nc(c3__cnzy, u2_blip);
    u2_noun wox = j2_mcy(Pt6, ut, mint)(van, sut, c3__noun, woo);
    u2_noun ret = u2_cn_nock_on(yib, u2k(u2t(wox)));

    if ( u2_none == ret ) {
      return u2_cm_error("bust-nock");
    }
    u2z(wox);
    u2z(woo);
    u2z(yam);

    return ret;
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, bust)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, bust)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, bust)(van, sut, dib);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, bust)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_weak hoc = u2_cj_look(van, "bust");

    if ( u2_none == hoc ) {
      c3_assert(!"register bust");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(dib), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, bust)[0].xip) ) {
        u2_noun xip = u2_cj_find(cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, bust)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, bust)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, bust)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, bust)(van, sut, dib);
      }
      else {
        c3_m    fun_m = c3__bust;
        u2_noun pro   = u2_ch_find_2(fun_m, sut, dib);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, bust)(van, sut, dib);

          return u2_ch_save_2(fun_m, sut, dib, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, bust)(van, sut, dib);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, bust)(
                        u2_noun cor)
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(dib));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, bust)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, bust),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, bust), c3__bust,
    },
    { }
  };
