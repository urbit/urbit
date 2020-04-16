/* vere/auto.c
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

/* u3_auto_init(): initialize all drivers
*/
void
u3_auto_init(u3_auto* car_u)
{
  while ( car_u ) {
    car_u->io.init_f(car_u);
    car_u = car_u->nex_u;
  }
}

/* u3_auto_talk(): start all drivers
*/
void
u3_auto_talk(u3_auto* car_u)
{
  while ( car_u ) {
    car_u->io.talk_f(car_u);
    car_u = car_u->nex_u;
  }
}

/* u3_auto_exit(): close all drivers
*/
void
u3_auto_exit(u3_auto* car_u)
{
  while ( car_u ) {
    car_u->io.exit_f(car_u);
    car_u = car_u->nex_u;
  }
}

/* u3_auto_live(): check if all drivers are live.
*/
c3_o
u3_auto_live(u3_auto* car_u)
{
  while ( car_u ) {
    if ( c3n == car_u->liv_o ) {
      return c3n;
    }

    car_u = car_u->nex_u;
  }

  return c3y;
}

/* u3_auto_plan(): create and enqueue an ovum
*/
u3_ovum*
u3_auto_plan(u3_auto* car_u,
             void*    vod_p,
             c3_l     msc_l,
             u3_noun    tar,
             u3_noun    pax,
             u3_noun    fav)
{
  u3_ovum* egg_u = c3_malloc(sizeof(*egg_u));
  egg_u->car_u = car_u;
  egg_u->vod_p = vod_p;
  egg_u->msc_l = msc_l;
  egg_u->tar   = tar;
  egg_u->pax   = pax;
  egg_u->fav   = fav;

  if ( !car_u->ent_u ) {
    c3_assert(!car_u->ext_u);

    egg_u->pre_u = egg_u->nex_u = 0;
    car_u->ent_u = car_u->ext_u = egg_u;
  }
  else {
    egg_u->nex_u = 0;
    egg_u->pre_u = car_u->ent_u;

    car_u->ent_u->nex_u = egg_u;
    car_u->ent_u = egg_u;
  }

  u3_pier_spin(car_u->pir_u);

  return egg_u;
}


/* u3_auto_drop(): dequeue and dispose an ovum.
*/
void
u3_auto_drop(u3_auto* car_u, u3_ovum* egg_u)
{
  if ( egg_u->pre_u ) {
    egg_u->pre_u->nex_u = egg_u->nex_u;
  }

  if ( egg_u->nex_u ) {
    egg_u->nex_u->pre_u = egg_u->pre_u;
  }

  //  notify driver if not self-caused
  //
  if ( egg_u->car_u && ( car_u != egg_u->car_u ) ) {
    egg_u->car_u->ev.drop_f(egg_u->car_u, egg_u->vod_p);
  }

  u3z(egg_u->tar);
  u3z(egg_u->pax);
  u3z(egg_u->fav);
  c3_free(egg_u);
}

/* u3_auto_next(): select an ovum and dequeue.
*/
u3_ovum*
u3_auto_next(u3_auto* car_u)
{
  u3_ovum* egg_u = 0;

  while ( car_u ) {
    if ( car_u->ext_u ) {
      egg_u = car_u->ext_u;

      c3_assert( !egg_u->pre_u );

      if ( egg_u->nex_u ) {
        egg_u->nex_u->pre_u = 0;
        car_u->ext_u = egg_u->nex_u;
      }
      else {
        car_u->ent_u = car_u->ext_u = 0;
      }

      egg_u->nex_u = 0;

      //  XX better name?
      //
      egg_u->car_u->ev.work_f(egg_u->car_u, egg_u->vod_p);

      return egg_u;
    }

    car_u = car_u->nex_u;    
  }

  return egg_u;
}

/* u3_auto_fete(): route effects to a linked driver
*/
void
u3_auto_fete(u3_auto* car_u, u3_noun act)
{
  u3_noun pax, fav, fec;
  u3_auto* rac_u = car_u;

  while ( u3_nul != act ) {
    fec = u3h(act);
    u3x_cell(fec, &pax, &fav);

    while ( c3n == car_u->io.fete_f(car_u, u3k(pax), u3k(fav)) ) {
      if ( !car_u->nex_u ) {
        // reck_kick_norm
        // "kick: lost"
        break;
      }
      else {
        car_u = car_u->nex_u;
      }
    }

    car_u = rac_u;
    act   = u3t(act);
  }
}
