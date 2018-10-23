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
  /*  there was an exponential backoff on timers, which meant that if you set a
   *  timer for every ~s1 inside urbit, each iteration would be slightly
   *  slower, up until ~25 timers had fired, at which point the exponential
   *  curve really kicked in and you wouldn't get a callback for ~m1 when you
   *  requested one in ~s1. nobody in the office knows why this is the way it
   *  is, and it goes back to the original behn code from 2014.
   */
  /*  u3_behn* teh_u = &u3_Host.teh_u;
   *  if(teh_u->run_w < 1024) {
   *    teh_u->run_w++;
   *  }
   */

  u3_lo_open();
  {
    u3v_plan
      (u3nt(u3_blip, c3__behn, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
  u3_lo_shut(c3n);
}

/* u3_behn_io_poll(): update behn IO state.
*/
void
u3_behn_io_poll(void)
{
  u3_behn* teh_u = &u3_Host.teh_u;
  u3_noun  wen   = u3v_keep(u3nt(u3_blip, c3__behn, u3_nul));

  if ( (u3_nul != wen) &&
       (c3y == u3du(wen)) &&
       (c3y == u3ud(u3t(wen))) )
  {
    c3_d gap_d = u3_time_gap_ms(u3k(u3A->now), u3k(u3t(wen)));

#if 0
    fprintf(stderr, "gap_d %llu, plus %llu\r\n", 
        gap_d, gap_d + (c3_d)teh_u->run_w);
#endif
    gap_d += teh_u->run_w;

    if ( c3y == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    else teh_u->alm = c3y;

    uv_timer_start(&teh_u->tim_u, _behn_time_cb, gap_d, 0);
  }
  else {
    if ( c3y == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    teh_u->alm = c3n;
  }
  u3z(wen);
}
