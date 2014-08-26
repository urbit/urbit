/* f/zock.c
**
** This file is in the public domain.
*/
#include "f/meme.h"


/* _cn_hint(): process hint.
*/
static u2_noun
_cn_hint(u2_noun zep, 
         u2_noun hod,
         u2_noun bus,
         u2_noun nex)
{
  switch ( zep ) {
    default: {
      u2_ca_lose(zep);
      u2_ca_lose(hod);

      return u2_ca_nock_on(bus, nex);
    }

    case c3__hunk:
    case c3__lose:
    case c3__mean:
    case c3__spot: {
      u2_noun tac = u2nc(zep, hod);
      u2_noun pro;

      u2_ct_push(tac);
#if 0
      if ( c3__spot == zep ) {
        printf("spot %d/%d : %d/%d\n",
               u2h(u2h(u2t(hod))),
               u2t(u2h(u2t(hod))),
               u2h(u2t(u2t(hod))),
               u2t(u2t(u2t(hod))));
      }
#endif
      pro = u2_ca_nock_on(bus, nex);
      u2_ct_drop();

      return pro;
    }

    case c3__slog: {
      u2_ct_slog(hod);
      return u2_ca_nock_on(bus, nex);
    }

    case c3__germ: {
      pro = u2_ca_nock_on(bus, nex);

      if ( u2_yes == u2_sing(pro, hod) ) {
        u2z(pro); return hod;
      } else {
        u2z(hod); return pro;
      }
    }

    case c3__fast: {
      u2_noun pro = u2_ca_nock_on(bus, nex);

      /* XX: translate hod to old clue form.
      */
      {
        u2_noun p_hod, q_hod, r_hod;

        if ( u2_no == u2_as_trel(hod, &p_hod, &q_hod, &r_hod) ) {
          u2z(hod);
          return pro;
        }
        else {
          u2_noun xod;

          if ( u2_yes == u2_dust(q_hod) &&
               (_1 == u2_h(q_hod)) &&
               (_0 == u2_t(q_hod)) ) {
            q_hod = 0;
          }
          xod = u2nt(u2k(q_hod), u2k(p_hod), u2k(r_hod));
          u2z(hod);
          hod = xod;
        }
      }
      return u2_cj_mine(hod, pro);
    }

    case c3__memo: {
      u2z(hod);
      {
        u2_noun pro = u2_cz_find_2(c3__nock, bus, nex);

        if ( pro != u2_none ) {
          u2z(bus); u2z(nex);
          return pro;
        }
        pro = u2_ca_nock_on(bus, nex);

        u2_cz_save_2(c3__nock, bus, nex, pro);
        u2z(bus); u2z(nex);
      }
    }

    case c3__sole: {
      u2z(hod);
      {
        u2_noun pro = u2_ca_nock_on(bus, nex);

        return u2_cz_uniq(pro);
      }
    }
  }
}

/* u2_cn_nock_on(): produce .*(bus fol).  Do not virtualize.
*/
u2_noun
u2_cn_nock_on(u2_noun bus, u2_noun fol)
{
  u2_noun hib, gal;

  while ( 1 ) {
    hib = u2h(fol);
    gal = u2t(fol);

    if ( u2_yes == u2_cr_du(hib) ) {
      u2_noun poz, riv;

      poz = u2_cn_nock_on(u2k(bus), u2k(hib));
      riv = u2_cn_nock_on(bus, u2k(gal));

      u2_ca_lose(fol);
      return u2_cn_cell(poz, riv);
    }
    else switch ( hib ) {
      default: return u2_cm_bail(c3__exit);

      case 0: {
        if ( u2_no == u2_cr_ud(gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun pro = u2k(u2at(gal, bus));

          u2_ca_lose(bus); u2_ca_lose(fol);
          return pro;
        }
      }
      c3_assert(!"not reached");

      case 1: {
        u2_noun pro = u2k(gal);

        u2_ca_lose(bus); u2_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 2: {
        u2_noun nex = u2_cn_nock_on(u2k(bus), u2k(u2t(gal)));
        u2_noun seb = u2_cn_nock_on(bus, u2k(u2h(gal)));

        u2_ca_lose(fol);
        bus = seb;
        fol = nex;
        continue;
      }
      c3_assert(!"not reached");

      case 3: {
        u2_noun gof, pro;

        gof = u2_cn_nock_on(bus, u2k(gal));
        pro = u2_cr_du(gof);

        u2_ca_lose(gof); u2_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 4: {
        u2_noun gof, pro;

        gof = u2_cn_nock_on(bus, u2k(gal));
        pro = u2_ci_vint(u2_Wire, gof);

        u2_ca_lose(gof); u2_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 5: {
        u2_noun wim = u2_cn_nock_on(bus, u2k(gal));
        u2_noun pro = u2_cr_sing(u2h(wim), u2t(wim));

        u2_ca_lose(wim); u2_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 6: {
        u2_noun b_gal, c_gal, d_gal;

        u2_cx_trel(gal, &b_gal, &c_gal, &d_gal);
        {
          u2_noun tys = u2_cn_nock_on(u2k(bus), u2k(b_gal));
          u2_noun nex;

          if ( 0 == tys ) {
            nex = u2k(c_gal);
          } else if ( 1 == tys ) {
            nex = u2k(d_gal);
          } else return u2_cm_bail(c3__exit);

          u2_ca_lose(fol);
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 7: {
        u2_noun b_gal, c_gal;

        u2_cx_cell(gal, &b_gal, &c_gal);
        {
          u2_noun bod = u2_cn_nock_on(bus, u2k(b_gal));
          u2_noun nex = u2k(c_gal);

          u2_ca_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 8: {
        u2_noun b_gal, c_gal;

        u2_cx_cell(gal, &b_gal, &c_gal);
        {
          u2_noun heb = u2_cn_nock_on(u2k(bus), u2k(b_gal));
          u2_noun bod = u2nc(heb, bus);
          u2_noun nex = u2k(c_gal);

          u2_ca_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 9: {
        u2_noun b_gal, c_gal;

        u2_cx_cell(gal, &b_gal, &c_gal);
        if ( u2_no == u2_cr_ud(b_gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun seb = u2_cn_nock_on(bus, u2k(c_gal));
          u2_weak xip;

          xip = u2_cj_find(u2k(seb));

          if ( u2_none != xip ) {
            u2_noun pro = u2_cj_kick(xip, seb, b_gal);

            u2_ca_lose(fol);
            return pro;
          }
          else {
            u2_noun nex = u2k(u2at(b_gal, seb));

            u2_ca_lose(fol);
            bus = seb;
            fol = nex;
            continue;
          }
        }
      }
      c3_assert(!"not reached");

      case 10: {
        u2_noun p_gal, q_gal;

        u2_cx_cell(gal, &p_gal, &q_gal);
        {
          u2_noun zep, hod, nex, pro;

          if ( u2_yes == u2_cr_du(p_gal) ) {
            u2_noun b_gal = u2h(p_gal);
            u2_noun c_gal = u2t(p_gal);
            u2_noun d_gal = q_gal;

            zep = u2k(b_gal);
            hod = u2_cn_nock_on(u2k(bus), u2k(c_gal));
            nex = u2k(d_gal);
          }
          else {
            u2_noun b_gal = p_gal;
            u2_noun c_gal = q_gal;

            zep = u2k(b_gal);
            hod = u2_nul;
            nex = u2k(c_gal);
          }

          u2_ca_lose(fol);
          pro = _cn_hint(zep, hod, bus, nex, 0);
          return pro;
        }
      }

      case 11: {
        c3_assert(!"11 remains stubbed out");
      }
      c3_assert(!"not reached");
    }
  }
}

/* u2_cn_kick_on(): fire `gat` without changing the sample.
*/
u2_noun
u2_cn_kick_on(u2_noun gat)
{
  if ( u2_none != (xip = u2_cj_find(gat)) ) {
    return u2_cj_kick(xip, gat, 2);
  } 
  else {
    return u2_cn_nock_on(gat, u2k(u2h(gat)));
  }
}

/* u2_cn_slam_on(): produce (gat sam).
*/
u2_noun
u2_cn_slam_on(u2_noun gat, u2_noun sam)
{
  u2_noun cor, xip;

  cor = u2nc(u2k(u2h(gat)), u2nc(sam, u2k(u2t(u2t(gat)))));
  u2z(gat);
  return u2_cn_kick_on(cor);
}

/* u2_cn_nock_un(): produce .*(bus fol), as ++toon.
*/
u2_noun
u2_cn_nock_un(u2_noun bus, u2_noun fol)
{
  u2_noun ton;

  u2_cm_leap();
  if ( u2_no == u2_cm_trap() ) {
    u2_noun ton;

    if ( 0 != u2R->net.nyd ) {
      ton = u2nc(1, u2R->net.nyd);
    } else {
      ton = u2nc(2, u2R->bug.tax);
    }
    u2_cm_fall();
    ton = u2_ca_gain(ton);
    u2_cm_flog(0);
  }
  else {
    u2_noun pro = u2_cn_nock_on(bus, fol);

    u2_cm_fall();
    ton = u2nc(0, u2_ca_gain(pro);
  }
  u2z(bus); u2z(fol); return ton;
}

/* u2_cn_slam_un(): produce (gat sam), as ++toon.
*/
u2_noun
u2_cn_slam_un(u2_noun gat, u2_noun sam)
{
  u2_noun ton;

  u2_cm_leap();
  if ( u2_no == u2_cm_trap() ) {
    u2_noun ton;

    if ( 0 != u2R->net.nyd ) {
      ton = u2nc(1, u2R->net.nyd);
    } else {
      ton = u2nc(2, u2R->bug.tax);
    }
    u2_cm_fall();
    ton = u2_ca_gain(ton);
    u2_cm_flog(0);
  }
  else {
    u2_noun pro = u2_cn_slam_on(gat, sam);

    u2_cm_fall();
    ton = u2nc(0, u2_ca_gain(pro);
  }
  u2z(gat); u2z(sam); return ton;
}

/* u2_cn_nock_in(): produce .*(bus fol), as ++toon, in namespace.
*/
u2_noun
u2_cn_nock_in(u2_noun fly, u2_noun bus, u2_noun fol)
{
  //  XX implement 11
  //
  u2z(fly); return u2_cn_nock_un(bus, fol);
}

/* u2_cn_slam_in(): produce (gat sam), as ++toon, in namespace.
*/
u2_noun
u2_cn_slam_in(u2_noun fly, u2_noun gat, u2_noun sam)
{
  //  XX implement 11
  //
  u2z(fly); return u2_cn_slam_un(bus, fol);
}

/* u2_cn_nock_an(): as slam_in(), but with empty fly.
*/
u2_noun
u2_cn_nock_an(u2_noun bus, u2_noun fol)
{
  return u2_cn_nock_un(bus, fol);
}
