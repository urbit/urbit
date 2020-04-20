/* vere/root.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* _root_io_talk():
*/
static void
_root_io_talk(u3_auto* car_u)
{
  u3_noun pax, fav;

  //  inject fresh entropy
  //
  {
    c3_w    eny_w[16];
    c3_rand(eny_w);

    pax = u3nt(u3_blip, c3__arvo, u3_nul);
    fav = u3nc(c3__wack, u3i_words(16, eny_w));

    u3_auto_plan(car_u, 0, 0, u3_blip, pax, fav);
  }

  //  set verbose as per -v
  //
  //    XX should be explicit, not a toggle
  //
  if ( c3y == u3_Host.ops_u.veb ) {
    //  XX this path shouldn't be necessary
    //
    pax = u3nq(u3_blip, c3__term, '1', u3_nul);
    fav = u3nc(c3__verb, u3_nul);

    u3_auto_plan(car_u, 0, 0, u3_blip, pax, fav);
  }
}

/* _root_io_fete():
*/
static c3_o
_root_io_fete(u3_auto* car_u, u3_noun pax, u3_noun fav)
{
  u3_noun i_pax, tag, dat;
  c3_o ret_o;

  if (  (c3n == u3r_cell(pax, &i_pax, 0))
     || (c3n == u3r_cell(fav, &tag, &dat))
     || (u3_blip  != i_pax ) )
  {
    ret_o = c3n;
  }
  else {
    switch ( tag ) {
      default: {
        ret_o = c3n;
      } break;

      case c3__exit: {
        ret_o = c3y;
        u3l_log("<<<goodbye>>>\n");
        u3_pier_exit(car_u->pir_u);
      } break;

      //  XX fake effect, check //arvo wire?
      //
      case c3__trim: {
        ret_o = c3y;
        u3_auto_plan(car_u, 0, 0, u3_blip, u3k(pax), u3k(fav));
      }

      case c3__vega: {
        ret_o = c3y;
        u3l_log("<<<reset>>>\n");
      } break;
    }
  }

  u3z(pax); u3z(fav);
  return ret_o;
}

/* _root_io_exit():
*/
static void
_root_io_exit(u3_auto* car_u)
{
  //  XX moveme
  //
  c3_l cod_l = u3a_lush(c3__save);
  u3_save_io_exit(car_u->pir_u);
  u3a_lop(cod_l);
}

static void
_root_ev_noop(u3_auto* car_u, void* vod_p)
{
}

/* u3_root_io_init():
*/
u3_auto*
u3_root_io_init(u3_pier* pir_u)
{
  u3_auto* car_u = c3_calloc(sizeof(*car_u));
  car_u->nam_m = c3__root;
  car_u->liv_o = c3n;
  car_u->io.talk_f = _root_io_talk;
  car_u->io.fete_f = _root_io_fete;
  car_u->io.exit_f = _root_io_exit;

  car_u->ev.drop_f = _root_ev_noop;
  car_u->ev.work_f = _root_ev_noop;
  car_u->ev.done_f = _root_ev_noop;
  car_u->ev.swap_f = _root_ev_noop;
  car_u->ev.bail_f = _root_ev_noop;

  //  XX moveme
  //
  {
    c3_l cod_l = u3a_lush(c3__save);
    u3_save_io_init(pir_u);
    u3a_lop(cod_l);
  }

  return car_u;
}
