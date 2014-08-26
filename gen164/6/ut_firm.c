/* j/6/firm.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, firm)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun:
      {
        return u2_yes;
      }
      case c3__void:
      {
        return u2_no;
      }
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: p_sut = u2t(sut);
      {
        return u2ud(dib);
      }
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_weak nun = u2_cr_at(u2h(u2t(u2t(p_sut))), dib);

        if ( u2_none == nun ) {
          return u2_no;
        } else {
          return u2_and(j2_mcx(Pt6, ut, firm)(van, q_sut, dib),
                        j2_mcx(Pt6, ut, firm)(van, u2t(u2t(u2t(p_sut))),
                                                          nun));
        }
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_and
          (u2du(dib),
           u2_and(j2_mcx(Pt6, ut, firm)(van, p_sut, u2h(dib)),
                  j2_mcx(Pt6, ut, firm)(van, q_sut, u2t(dib))));
      }
      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

        u2_cx_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
        u2_cx_cell(rq_sut, &prq_sut, &qrq_sut);

        return u2_and
          (u2du(dib),
           u2_and
            (j2_mcx(Pt6, ut, firm)(van, p_sut, u2h(dib)),
             ((u2_nul == prq_sut) ? u2_cm_error("firm-core")
                                  : u2_cr_sing(prq_sut, u2t(dib)))));
      }
      case c3__cube: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_cr_sing(dib, p_sut);
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return j2_mcx(Pt6, ut, firm)(van, q_sut, dib);
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_or(j2_mcx(Pt6, ut, firm)(van, p_sut, dib),
                     j2_mcx(Pt6, ut, firm)(van, q_sut, dib));
      }
      case c3__hold:
      {
        u2_noun goy = j2_mcy(Pt6, ut, repo)(van, sut);
        u2_noun ret = j2_mcx(Pt6, ut, firm)(van, goy, dib);

        u2z(goy);
        return ret;
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, firm)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, firm)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, firm)(van, sut, dib);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, firm)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "firm");

    if ( u2_none == hoc ) {
      c3_assert(!"register firm");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(dib), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, firm)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, firm)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, firm)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dib)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, firm)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, firm)(van, sut, dib);
      }
      else {
        c3_m    fun_m = c3__firm;
        u2_noun pro   = u2_cz_find_2(fun_m, sut, dib);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, firm)(van, sut, dib);

          return u2_cz_save_2(fun_m, sut, dib, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, firm)(van, sut, dib);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, firm)(
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
  j2_mcj(Pt6, ut, firm)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, firm),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, firm), c3__firm,
    },
    { }
  };
