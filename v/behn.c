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
#include "f/coal.h"
#include "v/vere.h"

#define u2R  ((u2_reck *) &u2_Host.arv_u)     //  new school

/* u2_behn_io_init(): initialize behn timer.
*/
void 
u2_behn_io_init(void)
{
  u2_behn* beh_u = &u2_Host.beh_u;

  uv_timer_init(u2L, &beh_u->tim_u);
  beh_u->alm = u2_no;
}

/* u2_behn_io_exit(): terminate timer.
*/
void 
u2_behn_io_exit(void)
{
}

/* _behn_time_cb(): timer callback.
*/
static void
_behn_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_lo_open();
  {
    u2_reck_plan
      (u2A,
       u2nt(c3__gold, c3__behn, u2_nul),
       u2nc(c3__wake, u2_nul));
  }
  u2_lo_shut(u2_no);
}

/* u2_behn_io_poll(): update behn IO state.
*/
void
u2_behn_io_poll(void)
{
  u2_behn* beh_u = &u2_Host.beh_u;
  u2_noun  wen   = u2_reck_keep(u2A, u2nt(c3__gold, c3__behn, u2_nul));
 
  if ( (u2_nul != wen) && 
       (u2_yes == u2du(wen)) &&
       (u2_yes == u2ud(u2t(wen))) )
  {
    c3_d gap_d = u2_time_gap_ms(u2k(u2A->now), u2k(u2t(wen)));

    if ( u2_yes == beh_u->alm ) {
      uv_timer_stop(&beh_u->tim_u);
    }
    else beh_u->alm = u2_yes;

    uv_timer_start(&beh_u->tim_u, _behn_time_cb, gap_d, 0);
  }
  else {
    if ( u2_yes == beh_u->alm ) {
      uv_timer_stop(&beh_u->tim_u);
    }
    beh_u->alm = u2_no;
  }
  u2z(wen);
}
