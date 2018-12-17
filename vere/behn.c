/* v/behn.c
**
**  This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* u3_behn(): initialize time timer.
*/
void
u3_behn_io_init(void)
{
  u3_behn* teh_u = &u3_Host.teh_u;

  uv_timer_init(u3L, &teh_u->tim_u);
  teh_u->alm = c3n;
}

/* u3_behn_io_exit(): terminate timer.
*/
void
u3_behn_io_exit(void)
{
}

/* _behn_time_cb(): timer callback.
*/
static void
_behn_time_cb(uv_timer_t* tim_u)
{
  u3_behn* teh_u = &u3_Host.teh_u;
  teh_u->alm = c3n;

  u3_lo_open();
  {
    u3v_plan
      (u3nt(u3_blip, c3__behn, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
  u3_lo_shut(c3n);
}

/* u3_behn_ef_doze(): set or cancel timer
*/
void
u3_behn_ef_doze(u3_noun wen)
{
  u3_behn* teh_u = &u3_Host.teh_u;

  struct timeval old_timer_tv = { 0, 0 };

  // If there was an previous timer, cancel it and save when it would have
  // fired.
  if ( c3y == teh_u->alm ) {
    old_timer_tv = teh_u->next_tv;

    uv_timer_stop(&teh_u->tim_u);
    teh_u->alm = c3n;
  }

  if ( (u3_nul != wen) &&
       (c3y == u3du(wen)) &&
       (c3y == u3ud(u3t(wen))) )
  {
    struct timeval now_tv;
    gettimeofday(&now_tv, 0);

    // Calculate the time when this timer should fire.
    struct timeval timer_tv;
    u3_time_out_tv(&timer_tv, u3k(u3t(wen)));

    // If there was a previous timer, and that timer would fire before the new
    // timer, use the previous timer's timeout.
    if ( timerisset(&old_timer_tv) &&
         timercmp(&old_timer_tv, &timer_tv, <) ) {
      timer_tv = old_timer_tv;
    }

    // Calculate how many milliseconds in the future uv should timeout.
    struct timeval future_ms_tv;
    timersub(&timer_tv, &now_tv, &future_ms_tv);

    c3_d gap_d = (future_ms_tv.tv_sec * 1000ULL) + (future_ms_tv.tv_usec / 1000);
    uv_timer_start(&teh_u->tim_u, _behn_time_cb, gap_d, 0);

    teh_u->alm = c3y;
    teh_u->next_tv = timer_tv;
  }

  u3z(wen);
}

/* u3_behn_ef_bake(): notify %behn that we're live
*/
void
u3_behn_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__behn, u3k(u3A->sen), u3_nul);

  u3v_plan(pax, u3nc(c3__born, u3_nul));
}
