/* v/save.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
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
_save_time_cb(uv_timer_t* tim_u)
{
  u3_save* sav_u = &u3_Host.sav_u;

  if ( sav_u->pid_w ) {
    return;
  }

  if ( u3A->ent_d > sav_u->ent_d ) {
    // uL(fprintf(uH, "autosaving... ent_d %" PRIu64 "\n", u3A->ent_d));

    // u3e_grab("save", u3_none);

    u3e_save();
    sav_u->ent_d = u3A->ent_d;
  }
}

/* u3_save_ef_chld(): report save termination.
*/
void
u3_save_ef_chld(void)
{
  u3_save* sav_u = &u3_Host.sav_u;
  c3_i     loc_i;
  c3_w     pid_w;

  /* modified for cases with no pid_w
  */
  uL(fprintf(uH, "checkpoint: complete %d\n", sav_u->pid_w));
  pid_w = wait(&loc_i);
  if (0 != sav_u->pid_w) {
    c3_assert(pid_w == sav_u->pid_w);
  }
  else {
    c3_assert(pid_w > 0);
  }
  sav_u->pid_w = 0;
}

/* u3_save_io_init(): initialize autosave.
*/
void
u3_save_io_init(void)
{
  u3_save* sav_u = &u3_Host.sav_u;

  sav_u->ent_d = 0;
  sav_u->pid_w = 0;

  uv_timer_init(u3L, &sav_u->tim_u);
  uv_timer_start(&sav_u->tim_u, _save_time_cb, 15000, 15000);
}

/* u3_save_io_exit(): terminate save I/O.
*/
void
u3_save_io_exit(void)
{
}

/* u3_save_io_poll(): poll kernel for save I/O.
*/
void
u3_save_io_poll(void)
{
}
