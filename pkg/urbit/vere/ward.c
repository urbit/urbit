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
