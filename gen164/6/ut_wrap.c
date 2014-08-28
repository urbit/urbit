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

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, wrap)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun yoz)                              //  retain
  {
    return j2_mcx(Pt6, ut, wrap)(van, sut, yoz);
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, wrap)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, wrap), Tier6_b, u2_none, u2_none },
    { }
  };
