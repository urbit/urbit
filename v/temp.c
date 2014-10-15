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
#include "f/coal.h"
#include "v/vere.h"

/* u2_temp(): initialize time timer.
*/
void
u2_temp_io_init(void)
{
  u2_temp* teh_u = &u2_Host.teh_u;

  uv_timer_init(u2L, &teh_u->tim_u);
  teh_u->alm = u2_no;
}

/* u2_temp_io_exit(): terminate timer.
*/
void
u2_temp_io_exit(void)
{
}

/* _temp_time_cb(): timer callback.
*/
static void
_temp_time_cb(uv_timer_t* tim_u)
{
  u2_temp* teh_u = &u2_Host.teh_u;
  if(teh_u->run_w < 1024) {
    teh_u->run_w++;
  }

  u2_lo_open();
  {
    u2_reck_plan
      (u2A,
       u2nt(u2_blip, c3__temp, u2_nul),
       u2nc(c3__wake, u2_nul));
  }
  u2_lo_shut(u2_no);
}

/* u2_temp_io_poll(): update temp IO state.
*/
void
u2_temp_io_poll(void)
{
  u2_temp* teh_u = &u2_Host.teh_u;
  u2_noun  wen   = u2_reck_keep(u2A, u2nt(u2_blip, c3__temp, u2_nul));

  if ( (u2_nul != wen) &&
       (u2_yes == u2du(wen)) &&
       (u2_yes == u2ud(u2t(wen))) )
  {
    c3_d gap_d = u2_time_gap_ms(u2k(u2A->now), u2k(u2t(wen)));

    gap_d += teh_u->run_w;

    if ( u2_yes == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    else teh_u->alm = u2_yes;

    uv_timer_start(&teh_u->tim_u, _temp_time_cb, gap_d, 0);
  }
  else {
    if ( u2_yes == teh_u->alm ) {
      uv_timer_stop(&teh_u->tim_u);
    }
    teh_u->alm = u2_no;
  }
  u2z(wen);
}
