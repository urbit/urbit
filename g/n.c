/* g/n.c
**
** This file is in the public domain.
*/
#include "all.h"

c3_w NOX = 0;

/* _cn_hint(): process hint.
*/
static u3_noun
_cn_hint(u3_noun zep, 
         u3_noun hod,
         u3_noun bus,
         u3_noun nex)
{
  switch ( zep ) {
    default: {
      u3_ca_lose(zep);
      u3_ca_lose(hod);

      return u3_cn_nock_on(bus, nex);
    }

    case c3__hunk:
    case c3__lose:
    case c3__mean:
    case c3__spot: {
      u3_noun tac = u3nc(zep, hod);
      u3_noun pro;

      u3_ct_push(tac);
#if 0
      if ( c3__spot == zep ) {
        printf("spot %d/%d : %d/%d\n",
               u3h(u3h(u3t(hod))),
               u3t(u3h(u3t(hod))),
               u3h(u3t(u3t(hod))),
               u3t(u3t(u3t(hod))));
      }
#endif
      pro = u3_cn_nock_on(bus, nex);
      u3_ct_drop();

      return pro;
    }

    case c3__slog: {
      u3_ct_slog(hod);
      return u3_cn_nock_on(bus, nex);
    }

    case c3__germ: {
      u3_noun pro = u3_cn_nock_on(bus, nex);

      if ( u3_yes == u3_cr_sing(pro, hod) ) {
        u3z(pro); return hod;
      } else {
        u3z(hod); return pro;
      }
    }

    case c3__fast: {
      u3_noun pro = u3_cn_nock_on(bus, nex);

      return u3_cj_mine(hod, pro);
    }

    case c3__memo: {
      u3z(hod);
      {
        u3_noun pro = u3_cz_find_2(c3__nock, bus, nex);

        if ( pro != u3_none ) {
          u3z(bus); u3z(nex);
          return pro;
        }
        pro = u3_cn_nock_on(bus, nex);

        u3_cz_save_2(c3__nock, bus, nex, pro);
        u3z(bus); u3z(nex);
      }
    }

    case c3__sole: {
      u3z(hod);
      {
        u3_noun pro = u3_cn_nock_on(bus, nex);

        return u3_cz_uniq(pro);
      }
    }
  }
}

/* u3_cn_nock_on(): produce .*(bus fol).  Do not virtualize.
*/
u3_noun
u3_cn_nock_on(u3_noun bus, u3_noun fol)
{
  u3_noun hib, gal;

  while ( 1 ) {
    hib = u3h(fol);
    gal = u3t(fol);

    NOX++;
    printf("# %d: %x, %x\r\n", NOX, u3_cr_mug(bus), u3_cr_mug(fol));

    if ( u3_yes == u3_cr_du(hib) ) {
      u3_noun poz, riv;

      poz = u3_cn_nock_on(u3k(bus), u3k(hib));
      riv = u3_cn_nock_on(bus, u3k(gal));

      u3_ca_lose(fol);
      return u3_ci_cell(poz, riv);
    }
    else switch ( hib ) {
      default: return u3_cm_bail(c3__exit);

      case 0: {
        if ( u3_no == u3_cr_ud(gal) ) {
          return u3_cm_bail(c3__exit);
        }
        else {
          u3_noun pro = u3k(u3at(gal, bus));

          u3_ca_lose(bus); u3_ca_lose(fol);
          return pro;
        }
      }
      c3_assert(!"not reached");

      case 1: {
        u3_noun pro = u3k(gal);

        u3_ca_lose(bus); u3_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 2: {
        u3_noun nex = u3_cn_nock_on(u3k(bus), u3k(u3t(gal)));
        u3_noun seb = u3_cn_nock_on(bus, u3k(u3h(gal)));

        u3_ca_lose(fol);
        bus = seb;
        fol = nex;
        continue;
      }
      c3_assert(!"not reached");

      case 3: {
        u3_noun gof, pro;

        gof = u3_cn_nock_on(bus, u3k(gal));
        pro = u3_cr_du(gof);

        u3_ca_lose(gof); u3_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 4: {
        u3_noun gof, pro;

        gof = u3_cn_nock_on(bus, u3k(gal));
        pro = u3_ci_vint(gof);

        u3_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 5: {
        u3_noun wim = u3_cn_nock_on(bus, u3k(gal));
        u3_noun pro = u3_cr_sing(u3h(wim), u3t(wim));

        u3_ca_lose(wim); u3_ca_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 6: {
        u3_noun b_gal, c_gal, d_gal;

        u3_cx_trel(gal, &b_gal, &c_gal, &d_gal);
        {
          u3_noun tys = u3_cn_nock_on(u3k(bus), u3k(b_gal));
          u3_noun nex;

          if ( 0 == tys ) {
            nex = u3k(c_gal);
          } else if ( 1 == tys ) {
            nex = u3k(d_gal);
          } else return u3_cm_bail(c3__exit);

          u3_ca_lose(fol);
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 7: {
        u3_noun b_gal, c_gal;

        u3_cx_cell(gal, &b_gal, &c_gal);
        {
          u3_noun bod = u3_cn_nock_on(bus, u3k(b_gal));
          u3_noun nex = u3k(c_gal);

          u3_ca_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 8: {
        u3_noun b_gal, c_gal;

        u3_cx_cell(gal, &b_gal, &c_gal);
        {
          u3_noun heb = u3_cn_nock_on(u3k(bus), u3k(b_gal));
          u3_noun bod = u3nc(heb, bus);
          u3_noun nex = u3k(c_gal);

          u3_ca_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 9: {
        u3_noun b_gal, c_gal;

        u3_cx_cell(gal, &b_gal, &c_gal);
        if ( u3_ne(u3_co_is_cat(b_gal)) ) {
          u3_cm_p("bad 9", fol);
        }

        {
          u3_noun seb = u3_cn_nock_on(bus, u3k(c_gal));
          u3_noun pro = u3_cj_kick(seb, b_gal);

          if ( u3_none != pro ) {
            u3_ca_lose(fol);
            return pro;
          }
          else {
            if ( u3_no == u3_cr_ud(b_gal) ) {
              return u3_cm_bail(c3__exit);
            }
            else {
              u3_noun nex = u3k(u3at(b_gal, seb));

              u3_ca_lose(fol);
              bus = seb;
              fol = nex;
              continue;
            }
          }
        }
      }
      c3_assert(!"not reached");

      case 10: {
        u3_noun p_gal, q_gal;

        u3_cx_cell(gal, &p_gal, &q_gal);
        {
          u3_noun zep, hod, nex;

          if ( u3_yes == u3_cr_du(p_gal) ) {
            u3_noun b_gal = u3h(p_gal);
            u3_noun c_gal = u3t(p_gal);
            u3_noun d_gal = q_gal;

            zep = u3k(b_gal);
            hod = u3_cn_nock_on(u3k(bus), u3k(c_gal));
            nex = u3k(d_gal);
          }
          else {
            u3_noun b_gal = p_gal;
            u3_noun c_gal = q_gal;

            zep = u3k(b_gal);
            hod = u3_nul;
            nex = u3k(c_gal);
          }

          u3_ca_lose(fol);
          return _cn_hint(zep, hod, bus, nex);
        }
      }

      case 11: {
        c3_assert(!"11 remains stubbed out");
      }
      c3_assert(!"not reached");
    }
  }
}

/* u3_cn_kick_on(): fire `gat` without changing the sample.
*/
u3_noun
u3_cn_kick_on(u3_noun gat)
{
  return u3_cj_kink(gat, 2);
}

/* u3_cn_slam_on(): produce (gat sam).
*/
u3_noun
u3_cn_slam_on(u3_noun gat, u3_noun sam)
{
  u3_noun cor = u3nc(u3k(u3h(gat)), u3nc(sam, u3k(u3t(u3t(gat)))));

  u3z(gat);
  return u3_cn_kick_on(cor);
}

/* u3_cn_nock_un(): produce .*(bus fol), as ++toon.
*/
u3_noun
u3_cn_nock_un(u3_noun bus, u3_noun fol)
{
  u3_noun ton;

  u3_cm_leap();
  if ( u3_blip != u3_cm_trap() ) {
    u3_noun ton;

    if ( 0 != u3R->net.nyd ) {
      ton = u3nc(1, u3R->net.nyd);
    } else {
      ton = u3nc(2, u3R->bug.tax);
    }
    u3_cm_fall();
    ton = u3_ca_gain(ton);
    u3_cm_flog(0);
  }
  else {
    u3_noun pro = u3_cn_nock_on(bus, fol);

    u3_cm_fall();
    ton = u3nc(0, u3_ca_gain(pro));
  }
  u3z(bus); u3z(fol); return ton;
}

/* u3_cn_slam_un(): produce (gat sam), as ++toon.
*/
u3_noun
u3_cn_slam_un(u3_noun gat, u3_noun sam)
{
  u3_noun ton;

  u3_cm_leap();
  if ( u3_blip != u3_cm_trap() ) {
    u3_noun ton;

    if ( 0 != u3R->net.nyd ) {
      ton = u3nc(1, u3R->net.nyd);
    } else {
      ton = u3nc(2, u3R->bug.tax);
    }
    u3_cm_fall();
    ton = u3_ca_gain(ton);
    u3_cm_flog(0);
  }
  else {
    u3_noun pro = u3_cn_slam_on(gat, sam);

    u3_cm_fall();
    ton = u3nc(0, u3_ca_gain(pro));
  }
  u3z(gat); u3z(sam); return ton;
}

/* u3_cn_nock_in(): produce .*(bus fol), as ++toon, in namespace.
*/
u3_noun
u3_cn_nock_in(u3_noun fly, u3_noun bus, u3_noun fol)
{
  //  XX implement 11
  //
  u3z(fly); return u3_cn_nock_un(bus, fol);
}

/* u3_cn_slam_in(): produce (gat sam), as ++toon, in namespace.
*/
u3_noun
u3_cn_slam_in(u3_noun fly, u3_noun gat, u3_noun sam)
{
  //  XX implement 11
  //
  u3z(fly); return u3_cn_slam_un(gat, sam);
}

/* u3_cn_nock_an(): as slam_in(), but with empty fly.
*/
u3_noun
u3_cn_nock_an(u3_noun bus, u3_noun fol)
{
  return u3_cn_nock_un(bus, fol);
}
