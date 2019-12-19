/* vere/behn.c
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

/* u3_behn(): initialize time timer.
*/
void
u3_behn_io_init(u3_pier *pir_u)
{
  u3_behn* teh_u = pir_u->teh_u;
  teh_u->alm = c3n;

  uv_timer_init(u3L, &teh_u->tim_u);
  teh_u->tim_u.data = pir_u;
}

/* u3_behn_io_exit(): terminate timer.
*/
void
u3_behn_io_exit(u3_pier *pir_u)
{
  u3_behn* teh_u = pir_u->teh_u;
  uv_close((uv_handle_t*)&teh_u->tim_u, 0);
}

/* _behn_time_cb(): timer callback.
*/
static void
_behn_time_cb(uv_timer_t* tim_u)
{
  u3_pier *pir_u = tim_u->data;
  u3_behn* teh_u = pir_u->teh_u;
  teh_u->alm = c3n;

  //  start another timer for 10 minutes
  //
  //  This is a backstop to deal with the case where a %doze is not
  //  properly sent, for example after a crash.  If the timer continues
  //  to fail, we can't proceed with the timers, but if it was a
  //  transient error, this will get us past it.
  //
  {
    c3_d gap_d = 10 * 60 * 1000;
    teh_u->alm = c3y;
    uv_timer_start(&teh_u->tim_u, _behn_time_cb, gap_d, 0);
  }

  // send timer event
  //
  {
    u3_pier_work
      (pir_u,
       u3nt(u3_blip, c3__behn, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
}

/* u3_behn_ef_doze(): set or cancel timer
*/
void
u3_behn_ef_doze(u3_pier *pir_u, u3_noun wen)
{
  u3_behn* teh_u = pir_u->teh_u;

  if ( c3y == teh_u->alm ) {
    uv_timer_stop(&teh_u->tim_u);
    teh_u->alm = c3n;
  }

  if ( (u3_nul != wen) &&
       (c3y == u3du(wen)) &&
       (c3y == u3ud(u3t(wen))) )
  {
    struct timeval tim_tv;
    gettimeofday(&tim_tv, 0);

    u3_noun now = u3_time_in_tv(&tim_tv);
    c3_d gap_d = u3_time_gap_ms(now, u3k(u3t(wen)));

    teh_u->alm = c3y;
    uv_timer_start(&teh_u->tim_u, _behn_time_cb, gap_d, 0);
  }

  u3z(wen);
}

/* u3_behn_ef_bake(): notify %behn that we're live
*/
void
u3_behn_ef_bake(u3_pier *pir_u)
{
  u3_noun pax = u3nq(u3_blip, c3__behn, u3k(u3A->sen), u3_nul);

  u3_pier_work(pir_u, pax, u3nc(c3__born, u3_nul));
}
