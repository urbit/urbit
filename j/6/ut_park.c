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
    if ( c3n == u3du(sut) || c3__core != u3h(sut) ) {
      return u3_cm_bail(c3__fail);
    }
    // else if ( c3n == u3_cj_hook(u3k(van), "vet") ) {
    else if ( c3n == u3_cr_at(u3_cqfu_van_vet, van) ) {
      return c3y;
    }
    else {
      u3_noun p_sut, q_sut, pq_sut;

      u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      u3_cx_cell(q_sut, &pq_sut, 0);

      if ( c3__both == way ) {
        if ( c3__gold == pq_sut ) {
          return c3y;
        } else return c3n;
      }
      if ( c3__read == way ) {
        switch ( pq_sut ) {
          default: return u3_cm_bail(c3__fail);

          case c3__gold: return c3y;
          case c3__lead: return c3n;
          case c3__iron: return c3n;
          case c3__zinc: return
                          c3a(!(u3_cr_sing(1, axe)),
                                 u3_cr_sing(2, u3_cqc_cap(axe)));
        }
      }
      else if ( c3__rite == way ) {
        switch ( pq_sut ) {
          default: return u3_cm_bail(c3__fail);

          case c3__gold: return c3y;
          case c3__lead: return c3n;
          case c3__iron: return
                          c3a(!(u3_cr_sing(1, axe)),
                                 u3_cr_sing(2, u3_cqc_cap(axe)));
          case c3__zinc: return c3n;
        }
      }
      else if ( c3__free == way ) { return c3y; }
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

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &way,
                                u3_cv_sam_3, &axe,
                                u3_cv_con, &van,
                                0)) ||
         (c3n == u3ud(axe)) ||
         (c3nne == (sut = u3_cr_at(u3_cv_sam, van))) )
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

