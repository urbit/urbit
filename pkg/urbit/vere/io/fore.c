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

    u3_auto_plan(car_u, 0, u3_blip, wir, cad);
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

    u3_auto_plan(car_u, 0, u3_blip, wir, cad);
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
