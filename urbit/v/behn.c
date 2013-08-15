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
#include <ev.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"

#define u2R  ((u2_reck *) &u2_Host.arv_u)     //  new school

static void _lo_behn(struct ev_loop *lup_u, struct ev_timer* tim_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, tim_u, c3__behn, rev_i); }
/* u2_behn_io_init(): initialize behn timer.
*/
void 
u2_behn_io_init(u2_reck* rec_u)
{
  u2_behn* beh_u = &u2_Host.beh_u;

  ev_timer_init(&beh_u->tim_u, _lo_behn, 10000.0, 0.);
  beh_u->alm = u2_no;
}

/* u2_behn_io_exit(): terminate timer.
*/
void 
u2_behn_io_exit(u2_reck* rec_u)
{
}

/* u2_behn_io_spin(): start behn timer.
*/
void
u2_behn_io_spin(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_behn* beh_u = &u2_Host.beh_u;

  if ( u2_yes == beh_u->alm ) {
    ev_timer_start(lup_u, &beh_u->tim_u);
  }
}

/* u2_behn_io_stop(): stop behn timer.
*/
void
u2_behn_io_stop(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_behn* beh_u = &u2_Host.beh_u;

  if ( u2_yes == beh_u->alm ) {
    ev_timer_stop(lup_u, &beh_u->tim_u);
  }
}

/* u2_behn_io_poll(): update behn IO state.
*/
void
u2_behn_io_poll(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_behn* beh_u = &u2_Host.beh_u;

  {
    u2_noun wen = u2_reck_keep(rec_u, u2nt(c3__gold, c3__behn, u2_nul));
    
    if ( (u2_nul != wen) && 
         (u2_yes == u2du(wen)) &&
         (u2_yes == u2ud(u2t(wen))) )
    {
      double gap_g = u2_time_gap_double(u2k(rec_u->now), u2k(u2t(wen)));

      beh_u->alm = u2_yes;
      ev_timer_set(&beh_u->tim_u, gap_g, 0.);
    }
    else {
      beh_u->alm = u2_no;
    }
  }
}

/* u2_behn_io_time(): time event on behn channel.
*/
void
u2_behn_io_time(u2_reck*         rec_u,
                struct ev_timer* tim_u)
{
  u2_reck_plan
    (rec_u,
     u2nt(c3__gold, c3__behn, u2_nul),
     u2nc(c3__wake, u2_nul));
}

