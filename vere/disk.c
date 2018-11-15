
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "vere/vere.h"


/* read */

c3_o
u3_disk_read_init(u3_pier* pir_u, c3_c * pot_c)
{
  fprintf(stderr, "disk.c: read init\r\n");

  pir_u->pin_u->log_u = c3_malloc(sizeof (u3_disk));
  memset( pir_u->pin_u->log_u, 0, sizeof (u3_disk));

  u3_disk * log_u = pir_u->pin_u->log_u;

  pir_u->pin_u->pos_d = 0; 
  log_u->fol_u = u3_foil_absorb(log_u->com_u, "commit.urbit-log");
  
  if ( !pir_u->pin_u->log_u->fol_u ) {
    return(c3n);
  }

  
  return(c3y);
}

c3_o 
u3_disk_read_read(u3_pier* pir_u, c3_y ** dat_y, c3_w * len_w, void ** hand_u)
{
  #if 0
  u3_disk * log_u = pir_u->pin_u->log_u;


  c3_w * data = (  c3_w *) u3_foil_reveal(log_u->fol_u, & log_u->pos_d, len_w);
  *hand_u = (void *) data;
#endif
  return(c3y);
}

void
u3_disk_read_done(void * hand_u)
{
  c3_free(hand_u);
}

void
u3_disk_read_shut(u3_pier * pir_u)
{

}

/* write */

c3_o
u3_disk_write_init(u3_pier* pir_u, c3_c * pot_c)
{
  fprintf(stderr, "disk.c: write init\r\n");

  u3_disk*  log_u = c3_malloc(sizeof(*log_u));
  
  memset(log_u, 0, sizeof(*log_u));
  log_u->pir_u = pir_u;
  pir_u->pot_u->log_u = log_u;

  /* create/load pier, urbit directory, log directory.
   */
  {
    /* pier directory
     */
    {
      if ( 0 == (log_u->dir_u = u3_foil_folder(pir_u->pax_c)) ) {
        return c3n;
      }
    }

    /* pier/.urb
     */
    {
      c3_c* urb_c = c3_malloc(6 + strlen(pir_u->pax_c));

      strcpy(urb_c, pir_u->pax_c);
      strcat(urb_c, "/.urb");

      if ( 0 == (log_u->urb_u = u3_foil_folder(urb_c)) ) {
        c3_free(urb_c);
        return c3n;
      }
      c3_free(urb_c);
    }

    /* pier/.urb/log
     */
    {
      c3_c* log_c = c3_malloc(10 + strlen(pir_u->pax_c));

      strcpy(log_c, pir_u->pax_c);
      strcat(log_c, "/.urb/log");

      if ( 0 == (log_u->com_u = u3_foil_folder(log_c)) ) {
        c3_free(log_c);
        return c3n;
      }
      c3_free(log_c);
    }

    /* pier/.urb/com
     */
    {
      c3_c* com_c = c3_malloc(10 + strlen(pir_u->pax_c));

      strcpy(com_c, pir_u->pax_c);
      strcat(com_c, "/.urb/com");

      if ( 0 == (log_u->com_u = u3_foil_folder(com_c)) ) {
        c3_free(com_c);
        return c3n;
      }
      c3_free(com_c);
    }

    /* pier/.urb/put and pier/.urb/get
     */
    {
      c3_c* dir_c = c3_malloc(10 + strlen(pir_u->pax_c));

      strcpy(dir_c, pir_u->pax_c);
      strcat(dir_c, "/.urb/put");
      mkdir(dir_c, 0700);

      strcpy(dir_c, pir_u->pax_c);
      strcat(dir_c, "/.urb/get");
      mkdir(dir_c, 0700);

      c3_free(dir_c);
    }
  }
  return c3y;
}

void
u3_disk_write_shut(u3_pier * pir_u)
{

}

void
_pier_disk_commit_complete(void* vod_p);


/* _pier_disk_commit_request(): start commit.
*/
void
u3_disk_write_write(u3_writ* wit_u,       /* IN: writ */
                    c3_d pos_d,           /* IN: row id */
                    c3_y* buf_y,          /* IN: buffer (to be freed later) */
                    c3_y* byt_y,          /* IN: data (located inside buffer above, but don't worry about that) */
                    c3_w  len_w,          /* IN: data len */
                    writ_test_cb test_cb          /* IN: void * (callback function) for testing - set NULL */
                     )
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_disk* log_u = pir_u->pot_u->log_u;

  //  fprintf(stderr, "pier: (%lld): commit: request\r\n", wit_u->evt_d);

  /* append to logfile
   */
  {
    c3_d  len_d = u3r_met(6, wit_u->mat);
    c3_d* buf_d = c3_malloc(8 * len_d);
 
    u3r_chubs(0, len_d, buf_d, wit_u->mat);
    u3_foil_append(_pier_disk_commit_complete,
                   wit_u,
                   log_u->fol_u,
                   buf_d, 
                   len_d);
  }

  wit_u->ped_o = c3y;
  free(buf_y);
}

void
u3_disk_write_shutdown(u3_pier* pir_u)
{

}



/* _pier_disk_commit_complete(): commit complete.
*/
void
_pier_disk_commit_complete(void* vod_p)
{
  u3_writ* wit_u = vod_p;
  u3_pier* pir_u = wit_u->pir_u;


  u3_pier_apply(pir_u);
}

