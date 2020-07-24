/* vere/root.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* _fore_inject_bail(): handle failure on arbitrary injection.
*/
static void
_fore_inject_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3_auto_bail_slog(egg_u, lud);
  u3l_log("pier: injected event failed\n");

  u3_ovum_free(egg_u);
}

/* _fore_inject(): inject an arbitrary ovum from a jammed file at [pax_c].
*/
static void
_fore_inject(u3_auto* car_u, c3_c* pax_c)
{
  //  XX soft
  //
  u3_noun ovo = u3ke_cue(u3m_file(pax_c));
  u3_noun riw, cad, tar, wir;

  if ( c3n == u3r_cell(ovo, &riw, &cad) ) {
    u3l_log("pier: invalid ovum in -I\n");
  }
  else if (  (c3n == u3a_is_cell(cad))
          || (c3n == u3a_is_atom(u3h(cad))) )
  {
    u3l_log("pier: invalid card in -I ovum\n");
  }
  else if ( c3n == u3r_cell(riw, &tar, &wir) ) {
    u3l_log("pier: invalid wire in -I ovum\n");
  }
  else if (  (c3n == u3a_is_atom(tar))
          || (1 < u3r_met(3, tar)) )
  {
    u3l_log("pier: invalid target in -I wire\n");
  }
  else {
    {
      c3_c* tag_c = u3r_string(u3h(cad));
      u3_noun ser = u3do("spat", u3k(riw));
      c3_c* wir_c = u3r_string(ser);

      u3l_log("pier: injecting %%%s event on %s\n", tag_c, wir_c);

      c3_free(tag_c);
      c3_free(wir_c);
      u3z(ser);
    }

    u3_auto_peer(
      u3_auto_plan(car_u, u3_ovum_init(0, u3k(tar), u3k(wir), u3k(cad))),
      0, 0, _fore_inject_bail);
  }

  u3z(ovo);
}

/* _fore_io_talk():
*/
static void
_fore_io_talk(u3_auto* car_u)
{
  u3_noun wir, cad;

  //  inject fresh entropy
  //
  {
    c3_w    eny_w[16];
    c3_rand(eny_w);

    wir = u3nc(c3__arvo, u3_nul);
    cad = u3nc(c3__wack, u3i_words(16, eny_w));

    u3_auto_plan(car_u, u3_ovum_init(0, u3_blip, wir, cad));
  }

  //  set verbose as per -v
  //
  //    XX should be explicit, not a toggle
  //
  if ( c3y == u3_Host.ops_u.veb ) {
    //  XX this path shouldn't be necessary
    //
    wir = u3nt(c3__term, '1', u3_nul);
    cad = u3nc(c3__verb, u3_nul);

    u3_auto_plan(car_u, u3_ovum_init(0, u3_blip, wir, cad));
  }

  //  inject arbitrary
  //
  if ( u3_Host.ops_u.jin_c ) {
    _fore_inject(car_u, u3_Host.ops_u.jin_c);
  }
}

/* _fore_io_kick(): handle no effects.
*/
static c3_o
_fore_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3z(wir); u3z(cad);
  return c3n;
}

/* _fore_io_exit():
*/
static void
_fore_io_exit(u3_auto* car_u)
{
  c3_free(car_u);
}

/* u3_fore_io_init(): initialize fore
*/
u3_auto*
u3_fore_io_init(u3_pier* pir_u)
{
  u3_auto* car_u = c3_calloc(sizeof(*car_u));
  car_u->nam_m = c3__fore;
  //  XX set in done_cb for %wack
  //
  car_u->liv_o = c3y;
  car_u->io.talk_f = _fore_io_talk;
  car_u->io.kick_f = _fore_io_kick;
  car_u->io.exit_f = _fore_io_exit;
  // car_u->ev.bail_f = ...;

  return car_u;
}
