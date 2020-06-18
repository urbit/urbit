/* vere/ward.c
*/
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

#include "all.h"
#include "vere/vere.h"

//  ward: lifecycle management for common structures
//
//    should contain anything allocated in multiple modules,
//    or allocated in one and freed in another
//

/* u3_dent_init(): initialize file record.
*/
u3_dent*
u3_dent_init(const c3_c* nam_c)
{
  u3_dent *det_u = c3_malloc(sizeof(*det_u));
  det_u->nex_u   = 0;
  det_u->nam_c   = c3_malloc(1 + strlen(nam_c));
  strcpy(det_u->nam_c, nam_c);

  return det_u;
}

/* u3_dent_free(): dispose file record.
*/
void
u3_dent_free(u3_dent *det_u)
{
  c3_free(det_u->nam_c);
  c3_free(det_u);
}

/* u3_dire_init(): initialize directory record.
*/
u3_dire*
u3_dire_init(const c3_c* pax_c)
{
  u3_dire *dir_u = c3_malloc(sizeof *dir_u);
  dir_u->all_u = 0;
  dir_u->pax_c = c3_malloc(1 + strlen(pax_c));
  strcpy(dir_u->pax_c, pax_c);

  return dir_u;
}

/* u3_dire_free(): dispose directory record.
*/
void
u3_dire_free(u3_dire *dir_u)
{
  {
    u3_dent *det_u = dir_u->all_u;
    u3_dent *nex_u;

    while ( det_u ) {
      nex_u = det_u->nex_u;
      u3_dent_free(det_u);
      det_u = nex_u;
    }
  }

  c3_free(dir_u->pax_c);
  c3_free(dir_u);
}

/* u3_fact_init(): initialize completed event.
*/
u3_fact*
u3_fact_init(c3_d eve_d, c3_l mug_l, u3_noun job)
{
  u3_fact *tac_u = c3_malloc(sizeof(*tac_u));
  tac_u->eve_d = eve_d;
  tac_u->mug_l = mug_l;
  tac_u->nex_u = 0;
  tac_u->job   = job;

  return tac_u;
}

/* u3_fact_free(): dispose completed event.
*/
void
u3_fact_free(u3_fact *tac_u)
{
  u3z(tac_u->job);
  c3_free(tac_u);
}

/* u3_gift_init(): initialize effect list.
*/
u3_gift*
u3_gift_init(c3_d eve_d, u3_noun act)
{
  u3_gift *gif_u = c3_malloc(sizeof(*gif_u));
  gif_u->eve_d = eve_d;
  gif_u->nex_u = 0;
  gif_u->act   = act;

  return gif_u;
}

/* u3_gift_free(): dispose effect list.
*/
void
u3_gift_free(u3_gift* gif_u)
{
  u3z(gif_u->act);
  c3_free(gif_u);
}
