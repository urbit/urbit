/* j/6/ut_park.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u3_bean
  _cqfu_park(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun way,
                        u3_atom axe)
  {
    if ( u3_no == u3du(sut) || c3__core != u3h(sut) ) {
      return u3_cm_bail(c3__fail);
    }
    // else if ( u3_no == u3_cj_hook(u3k(van), "vet") ) {
    else if ( u3_no == u3_cr_at(u3_cqfu_van_vet, van) ) {
      return u3_yes;
    }
    else {
      u3_noun p_sut, q_sut, pq_sut;

      u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      u3_cx_cell(q_sut, &pq_sut, 0);

      if ( c3__both == way ) {
        if ( c3__gold == pq_sut ) {
          return u3_yes;
        } else return u3_no;
      }
      if ( c3__read == way ) {
        switch ( pq_sut ) {
          default: return u3_cm_bail(c3__fail);

          case c3__gold: return u3_yes;
          case c3__lead: return u3_no;
          case c3__iron: return u3_no;
          case c3__zinc: return
                          u3_and(u3_not(u3_cr_sing(1, axe)),
                                 u3_cr_sing(2, u3_cqc_cap(axe)));
        }
      }
      else if ( c3__rite == way ) {
        switch ( pq_sut ) {
          default: return u3_cm_bail(c3__fail);

          case c3__gold: return u3_yes;
          case c3__lead: return u3_no;
          case c3__iron: return
                          u3_and(u3_not(u3_cr_sing(1, axe)),
                                 u3_cr_sing(2, u3_cqc_cap(axe)));
          case c3__zinc: return u3_no;
        }
      }
      else if ( c3__free == way ) { return u3_yes; }
      else return u3_cm_bail(c3__fail);
    }
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_park(
                       u3_noun cor)
  {
    u3_noun sut, way, axe, van;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &way,
                                u3_cv_sam_3, &axe,
                                u3_cv_con, &van,
                                0)) ||
         (u3_no == u3ud(axe)) ||
         (u3_none == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_park(van, sut, way, axe);
    }
  }

  u3_noun
  u3_cqfu_park(u3_noun van,
                        u3_noun sut,
                        u3_noun way,
                        u3_noun axe)
  {
    return _cqfu_park(van, sut, way, axe);
  }

