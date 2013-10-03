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
#include <uv.h>

#include "all.h"
#include "v/vere.h"

/* _save_time_cb(): timer callback.
*/
static void
_save_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_save* sav_u = &u2_Host.sav_u;

  if ( sav_u->pid_w ) {
    return;
  }

  if ( u2A->ent_w > sav_u->ent_w ) {
    c3_w pid_w;

    // uL(fprintf(uH, "autosaving... ent_w %d\n", u2A->ent_w));

    u2_cm_purge();
    // u2_ve_grab(0);

#ifdef FORKPT
    if ( 0 == (pid_w = fork()) ) {
      u2_loom_save(u2A->ent_w);
      exit(0);
    } 
    else {
      uL(fprintf(uH, "checkpoint: process %d\n", pid_w));

      sav_u->ent_w = u2A->ent_w;
      sav_u->pid_w = pid_w;
    }
#else
    u2_loom_save(u2A->ent_w);
    sav_u->ent_w = u2A->ent_w;
#endif
  } 
}

/* u2_save_ef_chld(): report save termination.
*/
void
u2_save_ef_chld(void)
{
  u2_save* sav_u = &u2_Host.sav_u;
  c3_i     loc_i;
  c3_w     pid_w;

  c3_assert(0 != sav_u->pid_w);
  uL(fprintf(uH, "checkpoint: complete %d\n", sav_u->pid_w));
  pid_w = wait(&loc_i);
  c3_assert(pid_w == sav_u->pid_w);

  sav_u->pid_w = 0;
}

/* u2_save_io_init(): initialize autosave.
*/
void 
u2_save_io_init(void)
{
  u2_save* sav_u = &u2_Host.sav_u;

  sav_u->ent_w = 0;
  sav_u->pid_w = 0;

  uv_timer_init(u2L, &sav_u->tim_u);
  uv_timer_start(&sav_u->tim_u, _save_time_cb, 5000, 5000);
}

/* u2_save_io_exit(): terminate save I/O.
*/
void 
u2_save_io_exit(void)
{
}

/* u2_save_io_poll(): poll kernel for save I/O.
*/
void
u2_save_io_poll(void)
{
}

