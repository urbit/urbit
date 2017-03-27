/* j/6/ut_conk.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqzu_conk(u3_noun van,
             u3_noun sut,
             u3_noun got)
  {
    if ( c3y == u3ud(got) ) {
      u3_noun nux = u3nc(u3_nul, u3k(got));
      u3_noun ret = u3qz_face(nux, sut);

      u3z(nux);
      return ret;
    }
    else switch ( u3h(got) ) {
      default: return u3m_bail(c3__fail);

      case 0: {
        return u3k(sut);
      }
      case 1: {
        u3_noun cok = _cqzu_conk(van, sut, u3t(u3t(got)));
        u3_noun nux = u3k(u3h(u3t(got)));
        u3_noun ret = u3qz_face(nux, cok);

        u3z(nux);
        u3z(cok);
        return ret;
      }
      case 2: {
        u3_noun vet = u3r_at(u3qzu_van_vet, van);
        u3_noun hed, tal, deh, lat, ret;

        if ( c3y == vet ) {
          u3_noun cel = u3nt(c3__cell, c3__noun, c3__noun);

          if ( c3n == u3qzu_nest(van, cel, c3y, sut) ) {
            return u3m_bail(c3__fail);
          }
          u3z(cel);
        }
        hed = u3qzu_peek(van, sut, c3__both, 2);
        tal = u3qzu_peek(van, sut, c3__both, 3);
        deh = _cqzu_conk(van, hed, u3h(u3t(got)));
        lat = _cqzu_conk(van, tal, u3t(u3t(got)));

        ret = u3qz_cell(deh, lat);

        u3z(lat);
        u3z(deh);
        u3z(tal);
        u3z(hed);

        return ret;
      }
    }
  }

/* boilerplate
*/
  u3_noun
  u3wzu_conk(u3_noun cor)
  {
    u3_noun sut, got, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &got,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqzu_conk(van, sut, got);
    }
  }

  u3_noun
  u3qzu_conk(u3_noun van,
             u3_noun sut,
             u3_noun got)
  {
    return _cqzu_conk(van, sut, got);
  }

