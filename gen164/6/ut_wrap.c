/* j/6/wrap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun                                                  //  produce
  j2_mcx(Pt6, ut, wrap)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2du(sut) )  {
      return u2_cm_error("wrap-type");
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_error("wrap-type");

      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun pq_sut, qq_sut, rq_sut;

        if ( u2_no == u2_cr_trel(q_sut, &pq_sut, &qq_sut, &rq_sut) ) {
          return u2_cm_bail(c3__fail);
        }
        else if ( c3__gold != pq_sut ) {
          return u2_cm_error("wrap-gold");
        }
        else {
          return u2nt(c3__core,
                              u2k(p_sut),
                              u2nt(u2k(yoz),
                                           u2k(qq_sut),
                                           u2k(rq_sut)));
        }
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return j2_mby(Pt6, fork)
          (j2_mcx(Pt6, ut, wrap)(van, p_sut, yoz),
                  j2_mcx(Pt6, ut, wrap)(van, q_sut, yoz));
      }
      case c3__hold:
      {
        u2_noun fop = j2_mcy(Pt6, ut, repo)(van, sut);
        u2_noun pro = j2_mcx(Pt6, ut, wrap)(van, fop, yoz);

        u2z(fop);
        return pro;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, wrap)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, wrap)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, yoz, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &yoz, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, wrap)(van, sut, yoz);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, wrap)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    u2_weak hoc = u2_cj_look(van, "wrap");

    if ( u2_none == hoc ) {
      c3_assert(!"register wrap");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(yoz), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, wrap)[0].xip) ) {
        u2_noun xip = u2_cj_find(cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, wrap)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, wrap)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, wrap)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, wrap)(van, sut, yoz);
      }
      else {
        c3_m    fun_m = c3__wrap;
        u2_noun pro   = u2_ch_find_2(fun_m, sut, yoz);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, wrap)(van, sut, yoz);

          return u2_ch_save_2(fun_m, sut, yoz, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, wrap)(van, sut, yoz);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, wrap)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, wrap), Tier6_b, u2_none, u2_none },
    { }
  };
