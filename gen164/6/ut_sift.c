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
  j2_mc(Pt6, ut, sift)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcy(Pt6, ut, sift)(van, sut, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, sift)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "sift");

    if ( u2_none == hoc ) {
      c3_assert(!"register sift");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(ref), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, sift)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        //  c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, sift)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, sift)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return u2k(ref);

#if 0
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, sift)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, sift)(van, sut, ref);
      }
      else {
        c3_m    fun_m = c3__sift;
        u2_noun pro   = u2_ch_find_3(fun_m, sut, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, sift)(van, sut, ref);

          return u2_ch_save_3(fun_m, sut, ref, pro);
        }
      }
      return pro;
    } else
#else
    {
      u2_noun cor, pro;

      cor = j2_mci(Pt6, ut, sift)(van, sut, ref);

      pro = u2_cn_nock_on(u2k(cor), u2k(u2h(cor)));
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);

      return pro;
#endif
    }
  }

  u2_weak
  j2_mck(Pt6, ut, sift)(
                        u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &ref,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc
        (u2k(sut), u2k(ref));
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

