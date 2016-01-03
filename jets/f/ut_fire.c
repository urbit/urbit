/* j/6/fire.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _fire_mull(u3_noun van,
             u3_noun sut,
             u3_noun dox,
             u3_noun gen)
  {
    u3_noun rib = u3r_at(u3qfu_van_rib, van);
    u3_noun key = u3nt(u3k(sut),
                       u3k(dox),
                       u3k(gen));
    u3_noun ret;

    if ( c3y == u3qdi_has(rib, key) ) {
      ret = c3y;
    }
    else {
      u3_noun rob = u3qdi_put(rib, key);
      u3_noun von = u3i_molt(u3k(van),
                             u3qfu_van_rib,
                             u3k(rob),
                             0);
      ret = u3qfu_mull(von, sut, c3__noun, dox, gen);

      u3z(von);
      u3z(rob);
    }
    u3z(key);
    return ret;
  }

  static u3_noun
  _fire_each(u3_noun van,
             u3_noun vet,
             u3_noun typ,
             u3_noun gat)
  {
    u3_noun p_typ, q_typ, pq_typ, qq_typ, rq_typ;
    u3_noun h_gat, t_gat;

    if ( (c3n == u3du(typ)) || (c3__core != u3h(typ)) ) {
      return u3m_error("fire-core");
    } else if
         ( (c3n == u3r_cell(u3t(typ), &p_typ, &q_typ)) ||
           (c3n == u3r_trel(q_typ, &pq_typ, &qq_typ, &rq_typ)) ||
           (c3n == u3r_cell(gat, &h_gat, &t_gat)) )
    {
      return u3m_bail(c3__fail);
    } else {
      u3_noun dox = u3nt
        (c3__core, u3k(qq_typ), u3k(q_typ));

      if ( c3__ash == u3h(gat) ) {
        if ( (c3y == vet) &&
             (c3n == u3qfu_nest(van, qq_typ, c3y, p_typ)) )
        {
#if 0
          u3_noun dun = u3qfu_dunq(van, "need", qq_typ);
          u3_noun niz = u3qfu_dunq(van, "have", p_typ);

          u3t_push(u3nc(c3__mean, niz));
          u3t_push(u3nc(c3__mean, dun));
#endif
          return u3m_error("fire-dry");
        }
        else {
          return u3nc(dox, u3k(t_gat));
        }
      }
      else {
        c3_assert(c3__elm == u3h(gat));
#if 0
        u3_noun dun = u3qfu_dunq(van, "wild", typ);
        u3_noun niz = u3qfu_dunq(van, "tame", dox);

        u3t_push(u3nc(c3__mean, dun));
        u3t_push(u3nc(c3__mean, niz));
#endif
        if ( (c3y == vet) &&
             (c3n == _fire_mull(van, typ, dox, t_gat)) )
        {
          return u3m_error("fire-wet");
        }
        else {
          u3z(dox);
#if 0
          u3t_drop();
          u3t_drop();
#endif
          return u3nc(u3k(typ), u3k(t_gat));
        }
      }
    }
  }

  static u3_noun
  _fire_in(u3_noun van,
           u3_noun vet,
           u3_noun hag)
  {
    if ( u3_nul == hag ) {
      return u3_nul;
    }
    else {
      u3_noun i_hag = u3h(hag);
      u3_noun t_hag = u3t(hag);

      if ( c3n == u3du(i_hag) ) {
        return u3m_bail(c3__fail);
      } else {
        return u3nc
          (_fire_each(van, vet, u3h(i_hag), u3t(i_hag)),
                      _fire_in(van, vet, t_hag));
      }
    }
  }
  u3_noun
  _cqfu_fire(u3_noun van,
             u3_noun sut,
             u3_noun hag)
  {
    u3_noun vet = u3r_at(u3qfu_van_vet, van);

    {
      if ( (c3y == u3du(hag)) && (u3_nul == u3t(hag)) ) {
        u3_noun i_hag = u3h(hag);
        u3_noun pi_hag = u3h(i_hag);
        u3_noun qi_hag = u3t(i_hag);

        if ( c3__elm == u3h(qi_hag) ) {
          u3_noun qqi_hag = u3t(qi_hag);

          if ( c3y == u3du(qqi_hag) &&
              (u3_nul == u3h(qqi_hag)) &&
              (1 == u3t(qqi_hag)) )
          {
            return u3k(pi_hag);
          }
        }
      }
    }
    return u3nc(c3__hold, _fire_in(van, vet, hag));
  }

/* boilerplate
*/
  u3_noun
  u3wfu_fire(u3_noun cor)
  {
    u3_noun sut, hag, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &hag, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fire(van, sut, hag);
    }
  }

  u3_noun
  u3qfu_fire(u3_noun van,
                        u3_noun sut,
                        u3_noun hag)
  {
    return _cqfu_fire(van, sut, hag);
  }
