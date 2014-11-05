/* v/temp.c
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
#include "v/vere.h"

/* u3_temp(): initialize time timer.
*/
void
u3_temp_io_init(void)
{
  u3_temp* teh_u = &u3_Host.teh_u;

  uv_timer_init(u3L, &teh_u->tim_u);
  teh_u->alm = c3n;
}

/* u3_temp_io_exit(): terminate timer.
*/
void
u3_temp_io_exit(void)
{
}

/* _temp_time_cb(): timer callback.
*/
static void
_temp_time_cb(uv_timer_t* tim_u)
{
  u3_temp* teh_u = &u3_Host.teh_u;
  if(teh_u->run_w < 1024) {
    teh_u->run_w++;
  }

  u3_lo_open();
  {
    u3_cv_plan
      (u3nt(u3_blip, c3__temp, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
  u3_lo_shut(c3n);
}

/* u3_temp_io_poll(): update temp IO state.
*/
void
u3_temp_io_poll(void)
{
  u3_temp* teh_u = &u3_Host.teh_u;
  u3_noun  wen   = u3_cv_keep(u3nt(u3_blip, c3__temp, u3_nul));

  if ( (u3_nul != wen) &&
       (c3y == u3du(wen)) &&
       (c3y == u3ud(u3t(wen))) )
  {
    c3_d gap_d = u3_time_gap_ms(u3k(u3A->now), u3k(u3t(wen)));

    gap_d += teh_u->run_w;

    if ( c3y == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    else teh_u->alm = c3y;

    uv_timer_start(&teh_u->tim_u, _temp_time_cb, gap_d, 0);
  }
  else {
    if ( c3y == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    teh_u->alm = c3n;
  }
  u3z(wen);
}
