/* vere/auto.c
*/
#include "all.h"
#include "vere/vere.h"

/* u3_auto_plan(): enqueue an ovum.
*/
u3_ovum*
u3_auto_plan(u3_auto* car_u, u3_ovum *egg_u)
{
  egg_u->car_u = car_u;

  if ( !car_u->ent_u ) {
    c3_assert(!car_u->ext_u);

    egg_u->pre_u = egg_u->nex_u = 0;
    car_u->ent_u = car_u->ext_u = egg_u;
    car_u->dep_w = 1;
  }
  //  enqueue at driver entry (back of the line)
  //
  //    [pre_u] points towards [ext_u] (back in time)
  //    [nex_u] points towards [ent_u] (forward in time)
  //
  else {
    egg_u->nex_u = 0;
    egg_u->pre_u = car_u->ent_u;

    car_u->ent_u->nex_u = egg_u;
    car_u->ent_u = egg_u;
    car_u->dep_w++;
  }

  u3_pier_spin(car_u->pir_u);

  return egg_u;
}

/* u3_auto_redo(): retry an ovum.
*/
u3_ovum*
u3_auto_redo(u3_auto* car_u, u3_ovum *egg_u)
{
  c3_assert( egg_u->car_u == car_u );

  egg_u->try_w++;

  if ( !car_u->ent_u ) {
    c3_assert(!car_u->ext_u);

    egg_u->pre_u = egg_u->nex_u = 0;
    car_u->ent_u = car_u->ext_u = egg_u;
    car_u->dep_w = 1;
  }
  //  enqueue at driver exit (front of the line)
  //
  else {
    egg_u->nex_u = car_u->ext_u;
    egg_u->pre_u = 0;

    car_u->ext_u->pre_u = egg_u;
    car_u->ext_u = egg_u;
    car_u->dep_w++;
  }

  u3_pier_spin(car_u->pir_u);

  return egg_u;
}

/* u3_auto_peer(): subscribe to updates.
*/
void
u3_auto_peer(u3_ovum*      egg_u,
             void*         ptr_v,
             u3_ovum_peer news_f,
             u3_ovum_bail bail_f)
{
  egg_u->ptr_v = ptr_v;
  egg_u->cb_u.news_f = news_f;
  egg_u->cb_u.bail_f = bail_f;
}

/* u3_auto_bail_slog(): print a bail notification.
*/
void
u3_auto_bail_slog(u3_ovum* egg_u, u3_noun lud)
{
  c3_c* car_c = u3r_string(egg_u->car_u->nam_m);
  u3_noun dul = lud;
  c3_w  len_w = 1;

  while ( u3_nul != dul ) {
    u3l_log("%s: bail %u", car_c, len_w++);
    u3_pier_punt_goof(car_c, u3k(u3h(dul)));

    dul = u3t(dul);
  }

  u3_pier_punt_ovum(car_c, u3k(egg_u->wir), u3k(u3h(egg_u->cad)));

  u3z(lud);
  c3_free(car_c);
}

/* u3_auto_bail(): notify driver that [egg_u] crashed.
*/
void
u3_auto_bail(u3_ovum* egg_u, u3_noun lud)
{
  //  optional
  //
  if ( egg_u->cb_u.bail_f ) {
    c3_l cod_l = u3a_lush(egg_u->car_u->nam_m);
    egg_u->cb_u.bail_f(egg_u, lud);
    u3a_lop(cod_l);
  }
  else {
    u3_auto_bail_slog(egg_u, lud);
    u3_ovum_free(egg_u);
  }
}

/* _auto_news(): notify driver of ovum status
*/
static void
_auto_news(u3_ovum* egg_u, u3_ovum_news new_e)
{
  // optional
  //
  if ( egg_u->cb_u.news_f ) {
    c3_l cod_l = u3a_lush(egg_u->car_u->nam_m);
    egg_u->cb_u.news_f(egg_u, new_e);
    u3a_lop(cod_l);
  }
}

/* u3_auto_done(): notify driver of [egg_u] completion.
*/
void
u3_auto_done(u3_ovum* egg_u)
{
  _auto_news(egg_u, u3_ovum_done);
  u3_ovum_free(egg_u);
}

/* u3_auto_work(): notify driver of [egg_u] commencement.
*/
void
u3_auto_work(u3_ovum* egg_u)
{
  _auto_news(egg_u, u3_ovum_work);
}

/* u3_auto_drop(): dequeue and dispose an ovum.
*/
void
u3_auto_drop(u3_auto* car_u, u3_ovum* egg_u)
{
  {
    c3_assert( egg_u->car_u );

    //  the previous ovum (or [ext_u]) will point to our next ovum
    //
    if ( !egg_u->pre_u ) {
      egg_u->car_u->ext_u = egg_u->nex_u;
    }
    else {
      egg_u->pre_u->nex_u = egg_u->nex_u;
    }

    //  the next ovum (or [ent_u]) will point to our previous ovum
    //
    if ( !egg_u->nex_u ) {
      egg_u->car_u->ent_u = egg_u->pre_u;
    }
    else {
      egg_u->nex_u->pre_u = egg_u->pre_u;
    }

    egg_u->car_u->dep_w--;

    egg_u->nex_u = egg_u->pre_u = 0;
  }

  //  notify driver if not self-caused
  //
  if ( egg_u->car_u && ( car_u != egg_u->car_u ) ) {
    _auto_news(egg_u, u3_ovum_drop);
  }

  u3_ovum_free(egg_u);
}

/* u3_auto_next(): select an ovum, dequeue and construct.
*/
u3_ovum*
u3_auto_next(u3_auto* car_u, u3_noun* ovo)
{
  while ( car_u ) {
    if ( !car_u->ext_u ) {
      car_u = car_u->nex_u;
      continue;
    }
    else {
      u3_ovum* egg_u = car_u->ext_u;

      c3_assert( !egg_u->pre_u );

      if ( egg_u->nex_u ) {
        egg_u->nex_u->pre_u = 0;
        car_u->ext_u = egg_u->nex_u;
        car_u->dep_w--;
      }
      else {
        car_u->ent_u = car_u->ext_u = 0;
        car_u->dep_w = 0;
      }

      egg_u->nex_u = 0;

      u3_auto_work(egg_u);

      *ovo = u3nc(u3nc(u3k(egg_u->tar), u3k(egg_u->wir)),
                  u3k(egg_u->cad));

      return egg_u;
    }
  }

  return 0;
}

/* _auto_kick_lost(): print details of unroutable effect. RETAIN
*/
static void
_auto_kick_lost(u3_noun pax, u3_noun fav)
{
  u3_noun tox = u3do("spat", u3k(pax));
  c3_c* tag_c = u3r_string(u3h(fav));
  c3_c* pax_c = u3r_string(tox);

  u3l_log("kick: lost %%%s on %s", tag_c, pax_c);

  c3_free(pax_c);
  c3_free(tag_c);
  u3z(tox);
}

/* _auto_kick(): kick with leak label.
*/
static c3_o
_auto_kick(u3_auto* car_u, u3_noun pax, u3_noun fav)
{
  c3_l cod_l = u3a_lush(car_u->nam_m);
  c3_o kik_o = car_u->io.kick_f(car_u, pax, fav);
  u3a_lop(cod_l);
  return kik_o;
}

/* u3_auto_kick(): route effects to a linked driver. RETAIN
*/
void
u3_auto_kick(u3_auto* car_u, u3_noun act)
{
  u3_auto* rac_u = car_u;
  u3_noun    fec, pax, wir, cad;

  while ( u3_nul != act ) {
    fec = u3h(act);
    u3x_cell(fec, &pax, &cad);

    //  XX temporary backwards compatibility, remove
    //
    if ( c3n == u3r_p(pax, u3_blip, &wir) ) {
      wir = pax;
    }

    while ( c3n == _auto_kick(car_u, u3k(wir), u3k(cad)) ) {
      if ( car_u->nex_u ) {
        car_u = car_u->nex_u;
        continue;
      }
      else {
        _auto_kick_lost(wir, cad);
        break;
      }
    }

    car_u = rac_u;
    act   = u3t(act);
  }
}

/* u3_auto_live(): check if all drivers are live.
*/
c3_o
u3_auto_live(u3_auto* car_u)
{
  while ( car_u ) {
    if ( c3n == car_u->liv_o ) {
      return c3n;
    }

    car_u = car_u->nex_u;
  }

  return c3y;
}

/* u3_auto_talk(): start all drivers.
*/
void
u3_auto_talk(u3_auto* car_u)
{
  c3_l cod_l;

  while ( car_u ) {
    cod_l = u3a_lush(car_u->nam_m);
    car_u->io.talk_f(car_u);
    u3a_lop(cod_l);
    car_u = car_u->nex_u;
  }
}

/* u3_auto_exit(): close all drivers.
*/
void
u3_auto_exit(u3_auto* car_u)
{
  u3_auto* nex_u;
  c3_l     cod_l;

  while ( car_u ) {
    nex_u = car_u->nex_u;

    {
      u3_ovum *egg_u = car_u->ext_u;
      u3_ovum *xen_u;

      while ( egg_u ) {
        xen_u = egg_u->nex_u;
        u3_ovum_free(egg_u);
        egg_u = xen_u;
      }
    }

    cod_l = u3a_lush(car_u->nam_m);
    car_u->io.exit_f(car_u);
    u3a_lop(cod_l);

    car_u = nex_u;
  }
}

/* u3_auto_info(): print status info.
*/
void
u3_auto_info(u3_auto* car_u)
{
  u3_auto* nex_u;

  u3l_log("  drivers:");

  while ( car_u ) {
    nex_u = car_u->nex_u;

    u3l_log("    %.*s: live=%s, queue=%u",
            u3r_met(3, car_u->nam_m),
            (c3_c*)&car_u->nam_m,
            ( c3y == car_u->liv_o ) ? "&" : "|",
            car_u->dep_w);

    //  XX details
    //
    if ( car_u->io.info_f ) {
      c3_l cod_l = u3a_lush(car_u->nam_m);
      car_u->io.info_f(car_u);
      u3a_lop(cod_l);
    }

    car_u = nex_u;
  }
}

/* _auto_link(): validate and link initalized [car_u]
*/
static u3_auto*
_auto_link(u3_auto* car_u, u3_pier* pir_u, u3_auto* nex_u)
{
  //  assert that io callbacks are present (info_f is optional)
  //
  c3_assert( car_u->io.talk_f );
  c3_assert( car_u->io.kick_f );
  c3_assert( car_u->io.exit_f );

  car_u->pir_u = pir_u;
  car_u->nex_u = nex_u;
  return car_u;
}

/* u3_auto_init(): initialize all drivers.
*/
u3_auto*
u3_auto_init(u3_pier* pir_u)
{
  u3_auto* car_u = 0;

  car_u = _auto_link(u3_hind_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_behn_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_ames_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_http_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_cttp_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_unix_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_term_io_init(pir_u), pir_u, car_u);
  car_u = _auto_link(u3_fore_io_init(pir_u), pir_u, car_u);

  return car_u;
}
