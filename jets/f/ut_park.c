/* j/6/ut_park.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_park(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_atom axe)
  {
    if ( c3n == u3du(sut) || c3__core != u3h(sut) ) {
      return u3m_bail(c3__fail);
    }
    // else if ( c3n == u3j_hook(u3k(van), "vet") ) {
    else if ( c3n == u3r_at(u3qfu_van_vet, van) ) {
      return c3y;
    }
    else {
      u3_noun p_sut, q_sut, pq_sut;

      u3x_cell(u3t(sut), &p_sut, &q_sut);
      u3x_cell(q_sut, &pq_sut, 0);

      if ( c3__both == way ) {
        if ( c3__gold == pq_sut ) {
          return c3y;
        } else return c3n;
      }
      if ( c3__read == way ) {
        switch ( pq_sut ) {
          default: return u3m_bail(c3__fail);

          case c3__gold: return c3y;
          case c3__lead: return c3n;
          case c3__iron: return c3n;
          case c3__zinc: return
                          c3a(!(u3r_sing(1, axe)),
                                u3r_sing(2, u3qc_cap(axe)));
        }
      }
      else if ( c3__rite == way ) {
        switch ( pq_sut ) {
          default: return u3m_bail(c3__fail);

          case c3__gold: return c3y;
          case c3__lead: return c3n;
          case c3__iron: return
                          c3a(!(u3r_sing(1, axe)),
                                u3r_sing(2, u3qc_cap(axe)));
          case c3__zinc: return c3n;
        }
      }
      else if ( c3__free == way ) { return c3y; }
      else return u3m_bail(c3__fail);
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_park(u3_noun cor)
  {
    u3_noun sut, way, axe, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &axe,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3ud(axe)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_park(van, sut, way, axe);
    }
  }

  u3_noun
  u3qfu_park(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun axe)
  {
    return _cqfu_park(van, sut, way, axe);
  }

