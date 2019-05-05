/* vere/save.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <termios.h>
#include <uv.h>

#include "all.h"
#include "vere/vere.h"

/* _save_time_cb(): timer callback.
*/
static void
_save_time_cb(uv_timer_t* tim_u)
{
  u3_pier *pir_u = tim_u->data;
  u3_pier_snap(pir_u);
}

/* u3_save_ef_chld(): report save termination.
*/
void
u3_save_ef_chld(u3_pier *pir_u)
{
  u3_save* sav_u = pir_u->sav_u;
  c3_i     loc_i;
  c3_w     pid_w;

  /* modified for cases with no pid_w
  */
  u3l_log("checkpoint: complete %d\n", sav_u->pid_w);
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
u3_save_io_init(u3_pier *pir_u)
{
  u3_save* sav_u = pir_u->sav_u;

  sav_u->req_d = 0;
  sav_u->dun_d = 0;
  sav_u->pid_w = 0;

  sav_u->tim_u.data = pir_u;
  uv_timer_init(u3L, &sav_u->tim_u);
  uv_timer_start(&sav_u->tim_u, _save_time_cb, 120000, 120000);
}

/* u3_save_io_exit(): terminate save I/O.
*/
void
u3_save_io_exit(u3_pier *pir_u)
{
}
