/* j/6/fink.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, fink)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    // u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "type", sut);
    u2_noun nuc = (u2_blip == cog)
      ? j2_mcy(Pt6, ut, shew)
          (van,
                  u2nc
                    (u2nc('c', u2_ci_string("find-limb")),
                            '$'))
      :  j2_mcy(Pt6, ut, shep)
          (van, "find-limb", 'a', u2k(cog));
    u2_noun pro;

    // u2_ct_push(u2nc(c3__mean, dun));
    u2_ct_push(u2nc(c3__mean, nuc));
    {
      u2_noun hoq = j2_mcy(Pt6, ut, find)(van, sut, dep, way, cog);
      u2_noun fin = u2t(hoq);

      if ( u2_nul == fin ) {
        return u2_cm_error("find-none");
      }
      else {
        pro = u2k(u2t(fin));
        u2z(hoq);
      }
    }
    u2_ct_drop();
    // u2_ct_drop();

    return pro;
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fink)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fink)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &dep,
                                u2_cv_sam_6, &way,
                                u2_cv_sam_7, &cog,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fink)(van, sut, dep, way, cog);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fink)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "fink");

    if ( u2_none == hoc ) {
      c3_assert(!"register fink");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam_2, u2k(dep),
                                      u2_cv_sam_6, u2k(way),
                                      u2_cv_sam_7, u2k(cog),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, fink)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fink)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fink)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fink)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, fink)(van, sut, dep, way, cog);
      }
      else {
        c3_m    fun_m = c3__fink;
        u2_noun pro   = u2_ch_find_4(fun_m, sut, dep, way, cog);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, fink)(van, sut, dep, way, cog);

          return u2_ch_save_4(fun_m, sut, dep, way, cog, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fink)(van, sut, dep, way, cog);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, fink)(
                        u2_noun cor)
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &dep,
                                u2_cv_sam_6, &way,
                                u2_cv_sam_7, &cog,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nq
        (u2k(sut),
                u2k(dep),
                u2k(way),
                u2k(cog));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fink)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, fink),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, fink), c3__fink,
    },
    { }
  };
