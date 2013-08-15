/* v/save.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <stdint.h>
#include <termios.h>
#include <ev.h>

#include "all.h"
#include "v/vere.h"

static void _lo_save(struct ev_loop *lup_u, struct ev_timer* tim_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, tim_u, c3__save, rev_i); }


/* u2_save_io_init(): initialize autosave.
*/
void 
u2_save_io_init(u2_reck* rec_u)
{
  u2_save* sav_u = &u2_Host.sav_u;

  sav_u->ent_w = 0;
  ev_timer_init(&sav_u->tim_u, _lo_save, 0.0, 0.);
}

/* u2_save_io_exit(): terminate save I/O.
*/
void 
u2_save_io_exit(u2_reck* rec_u)
{
}

/* u2_save_io_spin(): start save server(s).
*/
void
u2_save_io_spin(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_save* sav_u = &u2_Host.sav_u;

  ev_timer_start(lup_u, &sav_u->tim_u);
}

/* u2_save_io_stop(): stop save servers.
*/
void
u2_save_io_stop(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_save* sav_u = &u2_Host.sav_u;
  
  ev_timer_stop(lup_u, &sav_u->tim_u);
}

/* u2_save_io_poll(): update save IO state.
*/
void
u2_save_io_poll(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_save* sav_u = &u2_Host.sav_u;
  
  ev_timer_set(&sav_u->tim_u, 5.0, 0.);
}

/* u2_save_io_time(): time event on save channel.
*/
void
u2_save_io_time(u2_reck*         rec_u,
                struct ev_timer* tim_u)
{
  u2_save* sav_u = &u2_Host.sav_u;

  if ( rec_u->ent_w > sav_u->ent_w ) {
    // uL(fprintf(uH, "autosaving... ent_w %d\n", rec_u->ent_w));

    u2_cm_purge();
    u2_loom_save(rec_u->ent_w);
    sav_u->ent_w = rec_u->ent_w;
  }
}
