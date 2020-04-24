/* vere/pier.c
*/
#include <ent.h>
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

#define PIER_READ_BATCH 100ULL
#define PIER_PLAY_BATCH 10ULL
#define PIER_WORK_BATCH 10ULL

#undef VERBOSE_PIER

// XX snapshot timer

/* _pier_work_init(): begin processing new events
*/
static void
_pier_work_init(u3_pier* pir_u)
{
  pir_u->sat_e = u3_peat_work;
  u3_auto_talk(pir_u->car_u);
}

/* _pier_work_send(): send new events for processing
*/
static void
_pier_work_send(u3_pier* pir_u)
{
  u3_lord* god_u = pir_u->god_u;
  u3_auto* car_u = pir_u->car_u;
  c3_w     len_w = 0;

  //  calculate work batch size
  {
    u3_wall* wal_u = pir_u->wal_u;

    if ( !wal_u ) {
      if ( PIER_WORK_BATCH > god_u->dep_w ) {
        len_w = PIER_WORK_BATCH - god_u->dep_w;
      }
    }
    else {
      c3_d sen_d = god_u->eve_d + god_u->dep_w;
      if ( wal_u->eve_d > sen_d ) {
        len_w = wal_u->eve_d - sen_d;
      }
    }
  }

  //  send batch
  //
  {
    u3_ovum* egg_u;
    u3_noun    ovo;

    while (  len_w-- && car_u && (egg_u = u3_auto_next(car_u, &ovo)) ) {
      u3_lord_work(god_u, egg_u, ovo);

      //  queue events depth first
      //
      car_u = egg_u->car_u;
    }
  }
}

/* _pier_work_plan(): enqueue computed events, send to disk.
*/
static void
_pier_work_plan(u3_pier* pir_u, u3_work* wok_u)
{
  c3_assert( wok_u->eve_d > pir_u->wok_u.rel_d );
  c3_assert( wok_u->eve_d > pir_u->log_u->sen_d );

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): compute: complete\r\n", wok_u->eve_d);
#endif

  wok_u->nex_u = 0;

  if ( !pir_u->wok_u.ent_u ) {
    c3_assert( !pir_u->wok_u.ext_u );
    pir_u->wok_u.ent_u = pir_u->wok_u.ext_u = wok_u;
  }
  else {
    pir_u->wok_u.ent_u->nex_u = wok_u;
    pir_u->wok_u.ent_u = wok_u;
  }

  //  XX this is a departure from the general organization of this file
  //

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): disk: plan\r\n", wok_u->eve_d);
#endif
  u3_disk_plan(pir_u->log_u,
               wok_u->eve_d,
               wok_u->bug_l,
               wok_u->mug_l,
               u3k(wok_u->job));
}

/* _pier_work_next(): dequeue finished events for effect application
*/
static u3_work*
_pier_work_next(u3_pier* pir_u)
{
  u3_disk* log_u = pir_u->log_u;
  u3_work* wok_u = pir_u->wok_u.ext_u;

  if ( !wok_u || (wok_u->eve_d > log_u->dun_d) ) {
    return 0;
  }
  else {
    pir_u->wok_u.ext_u = wok_u->nex_u;

    if ( !pir_u->wok_u.ext_u ) {
      pir_u->wok_u.ent_u = 0;
    }

    c3_assert( (1ULL + pir_u->wok_u.rel_d) == wok_u->eve_d );
    pir_u->wok_u.rel_d = wok_u->eve_d;

    return wok_u;
  }
}

/* _pier_work_kick(): apply effects.
*/
static void
_pier_work_kick(u3_pier* pir_u)
{
  u3_work* wok_u;

  while ( (wok_u = _pier_work_next(pir_u)) ) {
#ifdef VERBOSE_PIER
    fprintf(stderr, "pier: (%" PRIu64 "): compute: release\r\n", wok_u->eve_d);
#endif
    u3_auto_kick(pir_u->car_u, wok_u->act);

    if ( wok_u->egg_u ) {
      u3_auto_drop(0, wok_u->egg_u);
    }

    //  XX dispose properly
    //
    c3_free(wok_u);
  }
}

/* _pier_work(): advance event processing.
*/
static void
_pier_work(u3_pier* pir_u)
{

  if ( c3n == pir_u->liv_o ) {
    pir_u->liv_o = u3_auto_live(pir_u->car_u);

    if ( c3y == pir_u->liv_o ) {
      //  XX print
      //  XX bot_f ?
    }
  }

  _pier_work_send(pir_u);
  _pier_work_kick(pir_u);
}

/* _pier_play_plan(): enqueue events for replay.
*/
static void
_pier_play_plan(u3_pier* pir_u, u3_play pay_u)
{
  u3_fact** ext_u;
  c3_d      old_d;

  if ( !pir_u->pay_u.ext_u ) {
    c3_assert( !pir_u->pay_u.ent_u );
    ext_u = &pir_u->pay_u.ext_u;
    old_d = pir_u->pay_u.sen_d;
  }
  else {
    ext_u = &pir_u->pay_u.ent_u->nex_u;
    old_d = pir_u->pay_u.ent_u->eve_d;
  }

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: play plan %" PRIu64 "-%" PRIu64 " at %" PRIu64 "\r\n",
                  pay_u.ext_u->eve_d,
                  pay_u.ent_u->eve_d,
                  old_d);
#endif

  c3_assert( (1ULL + old_d) == pay_u.ext_u->eve_d );

  *ext_u = pay_u.ext_u;
  pir_u->pay_u.ent_u = pay_u.ent_u;
}

/* _pier_play_send(): detach a batch of up to [len_d] events from queue.
*/
static u3_play
_pier_play_next(u3_pier* pir_u, c3_d len_d)
{
  u3_fact* tac_u = pir_u->pay_u.ext_u;
  u3_play  pay_u;

  //  set batch entry and exit pointers
  //
  {
    pay_u.ext_u = tac_u;

    while ( len_d-- && tac_u->nex_u ) {
      tac_u = tac_u->nex_u;
    }

    pay_u.ent_u = tac_u;
  }

  //  detatch batch from queue
  //
  if ( tac_u->nex_u ) {
    pir_u->pay_u.ext_u = tac_u->nex_u;
    tac_u->nex_u = 0;
  }
  else {
    pir_u->pay_u.ent_u = pir_u->pay_u.ext_u = 0;
  }

  return pay_u;
}

/* _pier_play_send(): send a batch of events to the worker for replay.
*/
static void
_pier_play_send(u3_pier* pir_u)
{
  //  XX fill the pipe how much?
  // (god_u->dep_w > PIER_WORK_BATCH) )
  //
  if ( pir_u->pay_u.ext_u ) {
    //  the first batch must be >= the lifecycle barrier
    //
    c3_d    len_d = ( !pir_u->pay_u.sen_d )
                    ? c3_max(pir_u->lif_w, PIER_PLAY_BATCH)
                    : PIER_PLAY_BATCH;
    u3_play pay_u = _pier_play_next(pir_u, len_d);

    //  bump sent counter
    //
    pir_u->pay_u.sen_d = pay_u.ent_u->eve_d;

#ifdef VERBOSE_PIER
    fprintf(stderr, "pier: play send %" PRIu64 "-%" PRIu64 "\r\n", pay_u.ext_u->eve_d, pay_u.ent_u->eve_d);
#endif

    u3_lord_play(pir_u->god_u, pay_u);
  }
}

/* _pier_play_read(): read events from disk for replay.
*/
static void
_pier_play_read(u3_pier* pir_u)
{
  c3_d las_d;

  if ( pir_u->pay_u.ent_u ) {
    las_d = pir_u->pay_u.ent_u->eve_d;

    //  cap the pir_u->pay_u queue depth
    //
    if ( (las_d - pir_u->pay_u.ext_u->eve_d) >= PIER_PLAY_BATCH ) {
      return;
    }
  }
  else {
    las_d = pir_u->pay_u.sen_d;
  }

  {
    c3_d nex_d = (1ULL + las_d);
    c3_d len_d = c3_min(pir_u->log_u->dun_d - las_d, PIER_READ_BATCH);

    if (  len_d
       && (nex_d > pir_u->pay_u.req_d) )
    {
      u3_disk_read(pir_u->log_u, nex_d, len_d);
      pir_u->pay_u.req_d = nex_d;

#ifdef VERBOSE_PIER
      fprintf(stderr, "pier: play read %" PRIu64 " at %" PRIu64 "\r\n", len_d, nex_d);
#endif
    }
  }
}

/* _pier_play_init(): begin boot/replay
*/
static void
_pier_play_init(u3_pier* pir_u)
{
  u3_lord* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;

  c3_assert( log_u->sen_d == log_u->dun_d );

  switch ( pir_u->sat_e ) {
    default: c3_assert(0);

    case u3_peat_init: {
      c3_assert( god_u->eve_d <= log_u->dun_d );
      pir_u->sat_e = u3_peat_play;
      pir_u->pay_u.sen_d = god_u->eve_d;

      u3l_log("---------------- playback starting ----------------\r\n");
      if ( (1ULL + god_u->eve_d) == log_u->dun_d ) {
        u3l_log("pier: replaying event %" PRIu64 "\r\n", log_u->dun_d);
      }
      else {
        u3l_log("pier: replaying events %" PRIu64 "-%" PRIu64 "\r\n",
                (1ULL + god_u->eve_d),
                log_u->dun_d);
      }

      u3_term_start_spinner(c3__play, c3y);
    } break;

    case u3_peat_boot: {
      c3_assert( !god_u->eve_d );
      u3l_log("---------------- boot starting ----------------\r\n");
      u3_term_start_spinner(c3__boot, c3y);
    } break;
  }
}

/* _pier_play(): send a batch of events to the worker for log replay.
*/
static void
_pier_play(u3_pier* pir_u)
{
  u3_lord* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;

  if ( log_u->sen_d > log_u->dun_d ) {
    //  wait if we're still committing the boot sequence
    //
    c3_assert( u3_peat_boot == pir_u->sat_e );
  }
  else if ( god_u->eve_d == log_u->dun_d ) {
    u3l_log("---------------- %s complete ----------------\r\n",
            ( u3_peat_boot == pir_u->sat_e ) ? "boot" : "playback");
    u3_term_stop_spinner();
    _pier_work_init(pir_u);
    // XX _pier_next(pir_u);
  }
  else {
    c3_assert( god_u->eve_d < log_u->dun_d );
    _pier_play_send(pir_u);
    _pier_play_read(pir_u);
  }
}

/* _pier_wall_plan(): enqueue a barrier.
*/
static void
_pier_wall_plan(u3_pier* pir_u, c3_d eve_d,
                void* vod_p, void (*wal_f)(void*, c3_d))
{
  u3_wall* wal_u = c3_malloc(sizeof(*wal_u));
  wal_u->vod_p = vod_p;
  wal_u->eve_d = eve_d;
  wal_u->wal_f = wal_f;

  //  insert into [pir_u->wal_u], preserving stable sort by [eve_d]
  //
  {
    u3_wall** las_u = &pir_u->wal_u;

    while ( *las_u && (eve_d <= (*las_u)->eve_d) ) {
      las_u = &(*las_u)->nex_u;
    }

    wal_u->nex_u = *las_u;
    *las_u = wal_u;
  }
}

/* _pier_wall(): process a barrier if possible.
*/
static void
_pier_wall(u3_pier* pir_u)
{
  u3_lord* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;

  if ( god_u->eve_d == log_u->dun_d ) {
    u3_wall* wal_u;

    //  XX check god_u->dep_w
    //
    while (  (wal_u = pir_u->wal_u)
          && !god_u->dep_w
          && (wal_u->eve_d <= god_u->eve_d) )
    {
      pir_u->wal_u = wal_u->nex_u;
      wal_u->wal_f(wal_u->vod_p, god_u->eve_d);
      c3_free(wal_u);
    }
  }
}

/* _pier_next(): advance the pier state machine.
*/
static void
_pier_next(u3_pier* pir_u)
{
  switch ( pir_u->sat_e ) {
    default: c3_assert(0);

    case u3_peat_work: {
      _pier_work(pir_u);
      break;
    }

    case u3_peat_play:
    case u3_peat_boot: {
      _pier_play(pir_u);
      break;
    }

    case u3_peat_done: {
      _pier_work_kick(pir_u);
      break;
    }
 
    case u3_peat_init: {
      break;
    }
  }

  _pier_wall(pir_u);
}

/* _pier_on_lord_slog(): debug printf from worker.
*/
static void
_pier_on_lord_slog(void* vod_p, c3_w pri_w, u3_noun tan)
{
  u3_pier* pir_u = vod_p;

  if ( c3y == u3a_is_atom(tan) ) {
    c3_c* tan_c = u3r_string(tan);
    u3C.stderr_log_f(tan_c);
    c3_free(tan_c);
    u3z(tan);
  }
  else {
    u3_pier_tank(0, pri_w, tan);
  }

  _pier_next(pir_u);
}

/* _pier_on_lord_peek(): namespace read response from worker.
*/
static void
_pier_on_lord_peek(void* vod_p, u3_noun gan, u3_noun pat, u3_noun dat);

/* _pier_on_lord_play_done(): log replay batch completion from worker.
*/
static void
_pier_on_lord_play_done(void* vod_p, u3_play pay_u, c3_l mug_l)
{
  u3_pier* pir_u = vod_p;
  c3_d     las_d = pay_u.ent_u->eve_d;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): play: done\r\n", las_d);
#endif

  //  XX optional
  //
  if (  pay_u.ent_u->mug_l
     && (pay_u.ent_u->mug_l != mug_l) )
  {
    //  XX printf
    //
    u3l_log("pier: (%" PRIu64 "): play: mug mismatch %x %x\r\n", las_d, pay_u.ent_u->mug_l, mug_l);
    // u3_pier_bail();
  }

  {
    u3_fact* tac_u = pay_u.ext_u;
    u3_fact* nex_u; 

    while ( tac_u ) {
      nex_u = tac_u->nex_u;
      u3z(tac_u->job);
      c3_free(tac_u);
      tac_u = nex_u;
    }
  }

  _pier_next(pir_u);
}

/* _pier_on_lord_play_bail(): log replay batch failure from worker.
*/
static void
_pier_on_lord_play_bail(void* vod_p, u3_play pay_u,
                        c3_l mug_l, c3_d eve_d, u3_noun dud)
{
#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): play: bail\r\n", eve_d);
#endif
  //  XX verify pay_u mug_l
  //  XX check dud mote, retry yap_u or shutdown

  // {
  //   u3_play* yap_u = c3_malloc(sizeof(*yap_u));
  //   u3_fact* fac_u = pay_u->ext_u;

  //   while ( fac_u->eve_d < eve_d ) {
  //     fac_u = fac_u->nex_u;
  //   }

  //   yap_u->ext_u = fac_u->nex_u;
  //   yap_u->ent_u = pay_u->ent_u;
  //   pay_u->ent_u = fac_u;
  // }

  u3_pier_bail();
}

/* _pier_on_lord_work_spin(): start spinner
*/
static void
_pier_on_lord_work_spin(void* vod_p, u3_atom pin, c3_o del_o)
{
  u3_term_start_spinner(pin, c3y); // (c3y == del_o) ? c3n : c3y);
}

/* _pier_on_lord_work_spin(): stop spinner
*/
static void
_pier_on_lord_work_spun(void* vod_p)
{
  u3_term_stop_spinner();
}

/* _pier_on_lord_work_done(): event completion from worker.
*/
static void
_pier_on_lord_work_done(void* vod_p, u3_work* wok_u, c3_o wap_o)
{
  u3_pier* pir_u = vod_p;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier (%" PRIu64 "): work: %s\r\n",
                  wok_u->eve_d,
                  ( c3y == wap_o ) ? "swap" : "done");
#endif

  u3_auto_done(wok_u->egg_u, wap_o);
  _pier_work_plan(pir_u, wok_u);
  _pier_next(pir_u);
}

/* _pier_on_lord_work_bail(): event failure from worker.
*/
static void
_pier_on_lord_work_bail(void* vod_p, u3_work* wok_u, u3_noun lud)
{
  u3_pier* pir_u = vod_p;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: work: bail\r\n");
#endif

  u3_auto_bail(wok_u->egg_u, lud);

  //  XX dispose wok_u
  //
  wok_u->egg_u = 0;

  _pier_next(pir_u);
}

/* _pier_on_lord_save(): worker state-export complete (portable snapshot).
*/
static void
_pier_on_lord_save(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): lord: snap\r\n", eve_d);
#endif

  _pier_next(pir_u);
}

/* _pier_on_lord_snap(): worker (non-portable) snapshot complete.
*/
static void
_pier_on_lord_snap(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): lord: snap\r\n", eve_d);
#endif

  _pier_next(pir_u);
}

/* _pier_on_lord_exit(): worker shutdown.
*/
static void
_pier_on_lord_exit(void* vod_p, c3_o ret_o)
{
  u3_pier* pir_u = vod_p;

  if ( u3_peat_done == pir_u->sat_e ) {
    //  XX dispose
    //
    // exit(0);
    u3_term_log_exit();
    //  XX no can do
    //
    uv_stop(u3L);
  }
  else {
    //  XX print error
    //  XX dispose
    u3_pier_bail();
  }
}

/* _pier_on_lord_live(): worker is ready.
*/
static void
_pier_on_lord_live(void* vod_p)
{
  u3_pier* pir_u = vod_p;
  u3_lord* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;

  //  XX plan kelvin event
  //

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): boot at mug %x\r\n", god_u->eve_d, god_u->mug_l);
#endif

  c3_assert( god_u->eve_d <= log_u->dun_d );

  if ( log_u->sen_d > log_u->dun_d ) {
    c3_assert( u3_peat_boot == pir_u->sat_e );
    //  will init on _disk_write_done
    //
  }
  else {
    c3_assert(  (u3_peat_boot == pir_u->sat_e)
             || (u3_peat_init == pir_u->sat_e) );

    if ( god_u->eve_d < log_u->dun_d ) {
      _pier_play_init(pir_u);
    }
    else {
      _pier_work_init(pir_u);
    }
  }

  _pier_next(pir_u);
}

/* _pier_on_disk_read_done(): event log read success.
*/
static void
_pier_on_disk_read_done(void* vod_p, u3_play pay_u)
{
  u3_pier* pir_u = vod_p;

  c3_assert(  (u3_peat_boot == pir_u->sat_e)
           || (u3_peat_play == pir_u->sat_e) );

  _pier_play_plan(pir_u, pay_u);

  _pier_next(pir_u);
}

/* _pier_on_disk_read_bail(): event log read failure.
*/
static void
_pier_on_disk_read_bail(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;

  // XX
  //  
  fprintf(stderr, "pier: disk read bail\r\n");
  u3_term_stop_spinner();
  u3_pier_bail();
}

/* _pier_on_disk_write_done(): event log write success.
*/
static void
_pier_on_disk_write_done(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;
  u3_disk* log_u = pir_u->log_u;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): db commit: complete\r\n", eve_d);
#endif

  if ( u3_peat_boot == pir_u->sat_e ) {
    pir_u->wok_u.rel_d = eve_d;

    //  wait if we're still committing the boot sequence
    //
    if ( log_u->sen_d == log_u->dun_d ) {
      _pier_play_init(pir_u);
    }
  }
  else {
    c3_assert( u3_peat_work == pir_u->sat_e );
  }

  _pier_next(pir_u);
}

/* _pier_on_disk_write_bail(): event log write failure.
*/
static void
_pier_on_disk_write_bail(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;

  // XX
  //
  fprintf(stderr, "pier: disk write bail\r\n");
  u3_pier_bail();
}

/* _pier_loop_time(): set time.
*/
static void
_pier_loop_time(u3_pier* pir_u)
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);

  // XX save to pier
  //
  u3v_time(u3_time_in_tv(&tim_tv));
}

/* _pier_loop_before_cb(): run on every loop iteration before i/o polling.
*/
static void
_pier_loop_fore_cb(uv_prepare_t* pep_u)
{
  u3_pier* pir_u = pep_u->data;
  _pier_loop_time(pir_u);
}

/* _pier_loop_afte_cb(): run on every loop iteration after i/o polling.
*/
static void
_pier_loop_afte_cb(uv_check_t* cek_u)
{
  u3_pier* pir_u = cek_u->data;
  _pier_next(pir_u);
}

/* _pier_loop_idle_cb(): run on next loop iteration.
*/
static void
_pier_loop_idle_cb(uv_idle_t* idl_u)
{
  u3_pier* pir_u = idl_u->data;
  _pier_next(pir_u);
  uv_idle_stop(idl_u);
}

/* u3_pier_spin(): (re-)activate idle handler
*/
void
u3_pier_spin(u3_pier* pir_u)
{
  if ( !uv_is_active((uv_handle_t*)&pir_u->idl_u) ) {
    uv_idle_start(&pir_u->idl_u, _pier_loop_idle_cb);
  }
}

static u3_auto*
_pier_loop_init(u3_pier* pir_u);

/* _pier_init(): create a pier, loading existing.
*/
static u3_pier*
_pier_init(c3_w wag_w, c3_c* pax_c)
{
  //  create pier
  //
  u3_pier* pir_u = c3_calloc(sizeof(*pir_u));

  pir_u->pax_c = pax_c;
  pir_u->sat_e = u3_peat_init;

  // XX remove
  //
  pir_u->por_s = u3_Host.ops_u.por_s;
  pir_u->sav_u = c3_calloc(sizeof(u3_save));

  //  initialize pre i/o polling handle
  //
  uv_prepare_init(u3L, &pir_u->pep_u);
  pir_u->pep_u.data = pir_u;
  uv_prepare_start(&pir_u->pep_u, _pier_loop_fore_cb);

  //  initialize post i/o polling handle
  //
  uv_check_init(u3L, &pir_u->cek_u);
  pir_u->cek_u.data = pir_u;
  uv_check_start(&pir_u->cek_u, _pier_loop_afte_cb);

  //  NB, not started
  //
  uv_idle_init(u3L, &pir_u->idl_u);
  pir_u->idl_u.data = pir_u;

  //  initialize persistence
  //
  {
    //  XX load/set secrets
    //
    u3_disk_cb cb_u = {
      .vod_p = pir_u,
      .read_done_f = _pier_on_disk_read_done,
      .read_bail_f = _pier_on_disk_read_bail,
      .write_done_f = _pier_on_disk_write_done,
      .write_bail_f = _pier_on_disk_write_bail
    };

    if ( !(pir_u->log_u = u3_disk_init(pax_c, cb_u)) ) {
      c3_free(pir_u);
      return 0;
    }

    pir_u->wok_u.rel_d = pir_u->log_u->dun_d;
  }

  //  start the worker process
  //
  {
    //  XX load/set secrets
    //
    c3_d tic_d[1];            //  ticket (unstretched)
    c3_d sec_d[1];            //  generator (unstretched)
    c3_d key_d[4];            //  secret (stretched)

    key_d[0] = key_d[1] = key_d[2] = key_d[3] = 0;

    u3_lord_cb cb_u = {
      .vod_p = pir_u,
      .live_f = _pier_on_lord_live,
      .spin_f = _pier_on_lord_work_spin,
      .spun_f = _pier_on_lord_work_spun,
      .slog_f = _pier_on_lord_slog,
      // .peek_f = _pier_on_lord_peek,
      .play_done_f = _pier_on_lord_play_done,
      .play_bail_f = _pier_on_lord_play_bail,
      .work_done_f = _pier_on_lord_work_done,
      .work_bail_f = _pier_on_lord_work_bail,
      .save_f = _pier_on_lord_save,
      .snap_f = _pier_on_lord_snap,
      .exit_f = _pier_on_lord_exit
    };

    if ( !(pir_u->god_u = u3_lord_init(pax_c, wag_w, key_d, cb_u)) )
    {
      // u3_disk_exit(pir_u->log_u)
      c3_free(pir_u);
      return 0;
    }
  }

  //  install in the pier table
  //
  //    XX  u3_king_plan
  //
  if ( 0 == u3K.all_w ) {
    u3K.all_w = 16;
    u3K.tab_u = c3_malloc(16 * sizeof(u3_pier*));
  }
  if ( u3K.len_w == u3K.all_w ) {
    u3K.all_w = 2 * u3K.all_w;
    u3K.tab_u = c3_realloc(u3K.tab_u, u3K.all_w * sizeof(u3_pier*));
  }
  u3K.tab_u[u3K.len_w++] = pir_u;

  return pir_u;
}

/* u3_pier_stay(): restart an existing pier.
*/
void
u3_pier_stay(c3_w wag_w, u3_noun pax)
{
  u3_pier* pir_u = _pier_init(wag_w, u3r_string(pax));

  if ( c3n == u3_disk_read_meta(pir_u->log_u,  pir_u->who_d,
                               &pir_u->fak_o, &pir_u->lif_w) )
  {
    fprintf(stderr, "pier: disk read meta fail\r\n");
    //  XX dispose
    //
    u3_pier_bail();
    exit(1);
  }

  pir_u->car_u = _pier_loop_init(pir_u);

  u3z(pax);
}

/* _pier_pill_parse(): extract boot formulas and module/userspace ova from pill
*/
static u3_boot
_pier_pill_parse(u3_noun pil)
{
  u3_boot bot_u;
  u3_noun pil_p, pil_q, pil_r;
  u3_noun pro;

  c3_assert( c3y == u3du(pil) );

  if ( c3y == u3h(pil) ) {
    u3x_trel(pil, 0, &pil_p, &pil_q);
  }
  else {
    u3x_qual(pil, 0, &pil_p, &pil_q, &pil_r);
  }

  pro = u3m_soft(0, u3ke_cue, u3k(pil_p));

  if ( 0 != u3h(pro) ) {
    fprintf(stderr, "boot: failed: unable to parse pill\r\n");
    exit(1);
  }

  u3x_trel(u3t(pro), &bot_u.bot, &bot_u.mod, &bot_u.use);
  u3k(bot_u.bot); u3k(bot_u.mod); u3k(bot_u.use);

  //  optionally replace filesystem in userspace
  //
  if ( c3y == u3h(pil) ) {
    if ( u3_nul != pil_q ) {
      c3_w len_w = 0;
      u3_noun ova = bot_u.use;
      u3_noun new = u3_nul;
      u3_noun ovo;

      while ( u3_nul != ova ) {
        ovo = u3h(ova);

        if ( c3__into == u3h(u3t(ovo)) ) {
          c3_assert( 0 == len_w );
          len_w++;
          ovo = u3t(pil_q);
        }

        new = u3nc(u3k(ovo), new);
        ova = u3t(ova);
      }

      c3_assert( 1 == len_w );

      u3z(bot_u.use);
      bot_u.use = u3kb_flop(new);
    }
  }
  //  prepend %lite module and userspace ova
  //
  else {
    bot_u.mod = u3kb_weld(u3k(pil_q), bot_u.mod);
    bot_u.use = u3kb_weld(u3k(pil_r), bot_u.use);
  }

  u3z(pro); u3z(pil);

  return bot_u;
}

/* _pier_boot_make(): construct boot sequence
*/
static u3_boot
_pier_boot_make(u3_noun who, u3_noun ven, u3_noun pil)
{
  u3_boot bot_u = _pier_pill_parse(pil); // transfer

  //  prepend entropy and identity to the module sequence
  //
  {
    u3_noun wir, cad;
    c3_w    eny_w[16];

    c3_rand(eny_w);
    wir = u3nt(u3_blip, c3__arvo, u3_nul);
    cad = u3nc(c3__wack, u3i_words(16, eny_w));
    bot_u.mod = u3nc(u3nc(wir, cad), bot_u.mod);

    wir = u3nt(u3_blip, c3__arvo, u3_nul);
    cad = u3nc(c3__whom, who);  // transfer
    bot_u.mod = u3nc(u3nc(wir, cad), bot_u.mod);
  }

  //  prepend legacy boot event to the userspace sequence
  //
  {
    //  XX do something about this wire
    //  XX route directly to %jael?
    //
    c3_assert( c3y == u3a_is_cell(ven) );

    u3_noun wir = u3nq(u3_blip, c3__term, '1', u3_nul);
    u3_noun cad = u3nt(c3__boot, u3_Host.ops_u.lit, ven); // transfer
    
    bot_u.use = u3nc(u3nc(wir, cad), bot_u.use);
  }

  return bot_u;
}

/* _pier_boot_plan(): construct and commit boot sequence
*/
static c3_o
_pier_boot_plan(u3_pier* pir_u, u3_noun who, u3_noun ven, u3_noun pil)
{
  u3_boot bot_u;
  {
    pir_u->sat_e = u3_peat_boot;
    pir_u->fak_o = ( c3__fake == u3h(ven) ) ? c3y : c3n;
    u3r_chubs(0, 2, pir_u->who_d, who);

    bot_u = _pier_boot_make(who, ven, pil);
    pir_u->lif_w = u3qb_lent(bot_u.bot);
  }

  if ( c3n == u3_disk_save_meta(pir_u->log_u, pir_u->who_d,
                                pir_u->fak_o, pir_u->lif_w) )
  {
    //  XX dispose bot_u
    //
    return c3n;
  }

  //  insert boot sequence directly
  //
  //    Note that these are not ovum or (pair @da ovum) events,
  //    but raw nock formulas to be directly evaluated as the
  //    subject of the lifecycle formula [%2 [%0 3] %0 2].
  //    All subsequent events will be (pair date ovum).
  //
  {
    u3_noun fol = bot_u.bot;

    while ( u3_nul != fol ) {
      u3_disk_boot_plan(pir_u->log_u, u3k(u3h(fol)));
      fol = u3t(fol);
    }
  }

  //  insert module and userspace events
  //
  //    XX increment [now] deterministically?
  //
  {
    struct timeval tim_tv;
    u3_noun ova = bot_u.mod;
    u3_noun now;

    while ( u3_nul != ova ) {
      gettimeofday(&tim_tv, 0);
      u3_disk_boot_plan(pir_u->log_u,
                        u3nc(u3_time_in_tv(&tim_tv),
                             u3k(u3h(ova))));
      ova = u3t(ova);
    }

    ova = bot_u.use;

    while ( u3_nul != ova ) {
      gettimeofday(&tim_tv, 0);
      u3_disk_boot_plan(pir_u->log_u,
                        u3nc(u3_time_in_tv(&tim_tv),
                             u3k(u3h(ova))));
      ova = u3t(ova);
    }
  }

  u3z(bot_u.bot);
  u3z(bot_u.mod);
  u3z(bot_u.use);

  return c3y;
}

/* u3_pier_boot(): start a new pier.
*/
void
u3_pier_boot(c3_w  wag_w,                   //  config flags
             u3_noun who,                   //  identity
             u3_noun ven,                   //  boot event
             u3_noun pil,                   //  type-of/path-to pill
             u3_noun pax)                   //  path to pier
{
  u3_pier* pir_u = _pier_init(wag_w, u3r_string(pax));

  if ( c3n == _pier_boot_plan(pir_u, who, ven, pil) ) {
    fprintf(stderr, "pier: boot plan fail\r\n");
    //  XX dispose
    //
    u3_pier_bail();
    exit(1);
  }

  pir_u->car_u = _pier_loop_init(pir_u);

  u3z(pax);
}

static void
_pier_save_cb(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;
  u3_lord_save(pir_u->god_u, eve_d);
}

/* u3_pier_save(): save a portable snapshot.
*/
void
u3_pier_save(u3_pier* pir_u)
{
  _pier_wall_plan(pir_u, 0, pir_u, _pier_save_cb);
}

static void
_pier_snap_cb(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;
  u3_lord_snap(pir_u->god_u, eve_d);
}

/* u3_pier_snap(): save a non-portable snapshot
*/
void
u3_pier_snap(u3_pier* pir_u)
{
  _pier_wall_plan(pir_u, 0, pir_u, _pier_snap_cb);
}

static void
_pier_exit_cb(void* vod_p, c3_d eve_d)
{
  u3_pier* pir_u = vod_p;
  u3_lord_exit(pir_u->god_u, 0);
}

/* u3_pier_exit(): shutdown.
*/
void
u3_pier_exit(u3_pier* pir_u)
{
  pir_u->sat_e = u3_peat_done;

  u3_pier_snap(pir_u);
  u3_disk_exit(pir_u->log_u);
  u3_auto_exit(pir_u->car_u);
  _pier_wall_plan(pir_u, 0, pir_u, _pier_exit_cb);
}

//  startup validation
//
//    replay the log
//    init all the i/o drivers
//


//  boot validation
//
//    play the (pill / boot-sequence)
//    init all the i/o drivers
//    neighbor with sponsor
//

/* _pier_loop_init_pier(): initialize loop handlers.
*/
static u3_auto*
_pier_loop_init(u3_pier* pir_u)
{

  _pier_loop_time(pir_u);

  //  for i/o drivers that still use u3A->sen
  //
  u3v_numb();

  return u3_auto_init(pir_u);
}

/* c3_rand(): fill a 512-bit (16-word) buffer.
*/
void
c3_rand(c3_w* rad_w)
{
  if ( 0 != ent_getentropy(rad_w, 64) ) {
    fprintf(stderr, "c3_rand getentropy: %s\n", strerror(errno));
    //  XX review
    //
    u3_pier_bail();
  }
}

/* _pier_exit_done(): synchronously shutting down
*/
static void
_pier_exit_done(u3_pier* pir_u)
{
  u3_disk_exit(pir_u->log_u);

  if ( 0 != pir_u->god_u ) {
    u3_lord_exit(pir_u->god_u, 0);
  }

  u3_auto_exit(pir_u->car_u);

  u3_term_log_exit();

  //  XX uninstall pier from u3K.tab_u, dispose

  //  XX no can do
  //
  uv_stop(u3L);
}

/* u3_pier_bail(): immediately shutdown.
*/
void
u3_pier_bail(void)
{
  if ( 0 != u3K.len_w ) {
    _pier_exit_done(u3_pier_stub());
  }

  fflush(stdout);
  exit(1);
}

/* _pier_dump_tape(): dump a tape, old style.  Don't do this.
*/
static void
_pier_dump_tape(FILE* fil_u, u3_noun tep)
{
  u3_noun tap = tep;

  while ( c3y == u3du(tap) ) {
    c3_c car_c;

    //  XX this utf-8 caution is unwarranted
    //
    //    we already write() utf8 directly to streams in term.c
    //
    // if ( u3h(tap) >= 127 ) {
    //   car_c = '?';
    // } else
    car_c = u3h(tap);

    putc(car_c, fil_u);
    tap = u3t(tap);
  }

  u3z(tep);
}

/* _pier_dump_wall(): dump a wall, old style.  Don't do this.
*/
static void
_pier_dump_wall(FILE* fil_u, u3_noun wol)
{
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    _pier_dump_tape(fil_u, u3k(u3h(wal)));

    putc(13, fil_u);
    putc(10, fil_u);

    wal = u3t(wal);
  }

  u3z(wol);
}

/* u3_pier_tank(): dump single tank.
*/
void
u3_pier_tank(c3_l tab_l, c3_w pri_w, u3_noun tac)
{
  u3_noun blu = u3_term_get_blew(0);
  c3_l  col_l = u3h(blu);
  FILE* fil_u = u3_term_io_hija();

  //  XX temporary, for urb.py test runner
  //
  if ( c3y == u3_Host.ops_u.dem ) {
    fil_u = stderr;
  }

  if ( c3n == u3_Host.ops_u.tem ) {
    switch ( pri_w ) {
        case 3: fprintf(fil_u, "\033[31m>>> "); break;
        case 2: fprintf(fil_u, "\033[33m>>  "); break;
        case 1: fprintf(fil_u, "\033[32m>   "); break;
    }
  }
  else {
    switch ( pri_w ) {
        case 3: fprintf(fil_u, ">>> "); break;
        case 2: fprintf(fil_u, ">>  "); break;
        case 1: fprintf(fil_u, ">   "); break;
    }
  }

  //  if we have no arvo kernel and can't evaluate nock
  //  only print %leaf tanks
  //
  if ( 0 == u3A->roc ) {
    if ( c3__leaf == u3h(tac) ) {
      _pier_dump_tape(fil_u, u3k(u3t(tac)));
      putc(13, fil_u);
      putc(10, fil_u);
    }
  }
  //  We are calling nock here, but hopefully need no protection.
  //
  else {
    u3_noun wol = u3dc("wash", u3nc(tab_l, col_l), u3k(tac));

    _pier_dump_wall(fil_u, wol);
  }

  if ( c3n == u3_Host.ops_u.tem ) {
    fprintf(fil_u, "\033[0m");
  }

  fflush(fil_u);

  u3_term_io_loja(0);
  u3z(blu);
  u3z(tac);
}

/* u3_pier_punt(): dump tank list.
*/
void
u3_pier_punt(c3_l tab_l, u3_noun tac)
{
  u3_noun cat = tac;

  while ( c3y == u3r_du(cat) ) {
    u3_pier_tank(tab_l, 0, u3k(u3h(cat)));
    cat = u3t(cat);
  }

  u3z(tac);
}

/* u3_pier_sway(): print trace.
*/
void
u3_pier_sway(c3_l tab_l, u3_noun tax)
{
  u3_noun mok = u3dc("mook", 2, tax);

  u3_pier_punt(tab_l, u3k(u3t(mok)));
  u3z(mok);
}

/* u3_pier_stub(): get the One Pier for unreconstructed code.
*/
u3_pier*
u3_pier_stub(void)
{
  if ( 0 == u3K.len_w ) {
    c3_assert(!"plan: no pier");
  }
  else {
    return u3K.tab_u[0];
  }
}

/* u3_pier_mark(): mark all Loom allocations in all u3_pier structs.
*/
c3_w
u3_pier_mark(FILE* fil_u)
{
  return 0;
}
