/* j/6/ut_park.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_bean                                                         //  transfer
  j2_mcx(Pt6, ut, park)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_atom axe)                              //  retain
  {
    if ( u2_no == u2du(sut) || c3__core != u2h(sut) ) {
      return u2_cm_bail(c3__fail);
    }
    // else if ( u2_no == u2_cj_hook(u2k(van), "vet") ) {
    else if ( u2_no == u2_cr_at(j2_ut_van_vet, van) ) {
      return u2_yes;
    }
    else {
      u2_noun p_sut, q_sut, pq_sut;

      u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      u2_cx_cell(q_sut, &pq_sut, 0);

      if ( c3__both == way ) {
        if ( c3__gold == pq_sut ) {
          return u2_yes;
        } else return u2_no;
      }
      if ( c3__read == way ) {
        switch ( pq_sut ) {
          default: return u2_cm_bail(c3__fail);

          case c3__gold: return u2_yes;
          case c3__lead: return u2_no;
          case c3__iron: return u2_no;
          case c3__zinc: return
                          u2_and(u2_not(u2_cr_sing(1, axe)),
                                 u2_cr_sing(2, j2_mbc(Pt3, cap)(axe)));
        }
      }
      else if ( c3__rite == way ) {
        switch ( pq_sut ) {
          default: return u2_cm_bail(c3__fail);

          case c3__gold: return u2_yes;
          case c3__lead: return u2_no;
          case c3__iron: return
                          u2_and(u2_not(u2_cr_sing(1, axe)),
                                 u2_cr_sing(2, j2_mbc(Pt3, cap)(axe)));
          case c3__zinc: return u2_no;
        }
      }
      else if ( c3__free == way ) { return u2_yes; }
      else return u2_cm_bail(c3__fail);
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, park)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, park)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, axe, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &way,
                                u2_cv_sam_3, &axe,
                                u2_cv_con, &van,
                                0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, park)(van, sut, way, axe);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, park)(u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun axe)                              //  retain
  {
    return j2_mcx(Pt6, ut, park)(van, sut, way, axe);
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, park)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, park), Tier6_b, u2_none, u2_none },
    { }
  };
