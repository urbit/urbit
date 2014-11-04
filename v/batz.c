/* v/batz.c
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

/* u3_batz_io_init(): initialize batz timer.
*/
void
u3_batz_io_init(void)
{
  u3_batz* beh_u = &u3_Host.beh_u;

  uv_timer_init(u3L, &beh_u->tim_u);
  beh_u->alm = u3_no;
}

/* u3_batz_io_exit(): terminate timer.
*/
void
u3_batz_io_exit(void)
{
}

/* _batz_time_cb(): timer callback.
*/
static void
_batz_time_cb(uv_timer_t* tim_u)
{
  u3_batz* beh_u = &u3_Host.beh_u;
  if(beh_u->run_w < 1024) {
    beh_u->run_w++;
  }

  u3_lo_open();
  {
    u3_cv_plan
      (u3nt(u3_blip, c3__batz, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
  u3_lo_shut(u3_no);
}

/* u3_batz_io_poll(): update batz IO state.
*/
void
u3_batz_io_poll(void)
{
  u3_batz* beh_u = &u3_Host.beh_u;
  u3_noun  wen   = u3_cv_keep(u3nt(u3_blip, c3__batz, u3_nul));

  if ( (u3_nul != wen) &&
       (u3_yes == u3du(wen)) &&
       (u3_yes == u3ud(u3t(wen))) )
  {
    c3_d gap_d = u3_time_gap_ms(u3k(u3A->now), u3k(u3t(wen)));

    gap_d += beh_u->run_w;

    if ( u3_yes == beh_u->alm ) {
      uv_timer_stop(&beh_u->tim_u);
    }
    else beh_u->alm = u3_yes;

    uv_timer_start(&beh_u->tim_u, _batz_time_cb, gap_d, 0);
  }
  else {
    if ( u3_yes == beh_u->alm ) {
      uv_timer_stop(&beh_u->tim_u);
    }
    beh_u->alm = u3_no;
  }
  u3z(wen);
}
