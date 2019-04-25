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
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include "vere/vere.h"

#undef VERBOSE_EVENTS

  /*    event handling proceeds on a single path. across both the
  **    child worker process (worker) and parent i/o process (daemon).
  **    state transitions are as follows:
  **
  **        generated               (event numbered and queued)
  **        dispatched              (sent to worker)
  **        computed                (completed by worker)
  **        commit requested        (sent to storage subsystem)
  **        commit complete         (daemon notified)
  **        released                (output actions allowed)
  **
  **    we dispatch one event at a time to the worker.  we don't do
  **    anything in parallel.
  **
  **    the sanity constraints that constrain this path:
  **
  **        - an event can't request a commit until it's computed.
  **        - an event can't be released until it, and all events
  **          preceding it, are computed and committed.
  **
  **    event numbers are uint64 (c3_d) which start with 1.  we order
  **    events as we receive them.
  **
  **    events are executed in order by the working process, and
  **    (at present) committed in strict order.
  **
  **    the result of computing an event can be completion (in which
  **    case we go directly to commit) or replacement (in which we
  **    replace the input event with a different event).
  **
  **    after crash recovery, events committed but not in the snapshot
  **    (the state of the worker) are replayed (re-computed), but their
  **    output effects are ignored. it is possible that effects of
  **    (only the last of ?) these events are not completely released to
  **    the outside world -- but they should never be released more than once.
  **
  **    XX analyze replay more comprehensively
  */

static void _pier_apply(u3_pier* pir_u);
static void _pier_boot_complete(u3_pier* pir_u);
static void _pier_boot_ready(u3_pier* pir_u);
static void _pier_boot_set_ship(u3_pier* pir_u, u3_noun who, u3_noun fak);
static void _pier_exit_done(u3_pier* pir_u);

/* _pier_db_bail(): bail from disk i/o.
*/
static void
_pier_db_bail(void* vod_p, const c3_c* err_c)
{
  u3l_log("disk error: %s\r\n", err_c);
}

/* _pier_db_shutdown(): close the log.
*/
static void
_pier_db_shutdown(u3_pier* pir_u)
{
  u3_lmdb_shutdown(pir_u->log_u->db_u);
}

/* _pier_db_commit_complete(): commit complete.
 */
static void
_pier_db_commit_complete(c3_o success, u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_disk* log_u = pir_u->log_u;

  if (success == c3n) {
    u3l_log("Failed to persist event. Exiting to prevent corruption.");
    u3_pier_bail();
  }

#ifdef VERBOSE_EVENTS
  u3l_log("pier: (%" PRIu64 "): db commit completed\r\n", wit_u->evt_d);
#endif

  /* advance commit counter
  */
  {
    c3_assert(wit_u->evt_d == log_u->moc_d);
    c3_assert(wit_u->evt_d == (1ULL + log_u->com_d));
    log_u->com_d += 1ULL;
  }

  _pier_apply(pir_u);
}

/* _pier_db_commit_request(): start commit.
*/
static void
_pier_db_commit_request(u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_disk* log_u = pir_u->log_u;

#ifdef VERBOSE_EVENTS
  u3l_log("pier: (%" PRIu64 "): commit: request\r\n", wit_u->evt_d);
#endif

  /* put it in the database
  */
  {
    u3_lmdb_write_event(log_u->db_u,
                        wit_u,
                        _pier_db_commit_complete);
  }

  /* advance commit-request counter
  */
  {
    c3_assert(wit_u->evt_d == (1ULL + log_u->moc_d));
    log_u->moc_d += 1ULL;
  }
}


static void
_pier_db_write_header(u3_pier* pir_u,
                      u3_noun who,
                      u3_noun is_fake,
                      u3_noun life)
{
  c3_o ret = u3_lmdb_write_identity(pir_u->log_u->db_u,
                                    who, is_fake, life);
  if (ret == c3n) {
    u3_pier_bail();
  }
}

/* _pier_db_read_header(): reads the ships metadata from lmdb
 */
static void
_pier_db_read_header(u3_pier* pir_u)
{
  u3_noun who, is_fake, life;
  c3_o ret = u3_lmdb_read_identity(pir_u->log_u->db_u,
                                   &who, &is_fake, &life);
  if (ret == c3n) {
    u3l_log("Failed to load identity. Exiting...");
    u3_pier_bail();
  }

  _pier_boot_set_ship(pir_u, u3k(who), u3k(is_fake));
  pir_u->lif_d = u3r_chub(0, life);

  u3z(who);
  u3z(is_fake);
  u3z(life);
}

static c3_o
_pier_db_on_commit_loaded(u3_pier* pir_u,
                          c3_d id,
                          u3_noun mat)
{
  // Need to grab references to the nouns above.
  u3_writ* wit_u = c3_calloc(sizeof(u3_writ));
  wit_u->pir_u = pir_u;
  wit_u->evt_d = id;
  wit_u->mat = u3k(mat);

  // Parse the expected mug_l and job out of mat.
  u3_noun entry = u3ke_cue(u3k(mat));
  u3_noun mug, job;
  if ( (c3y != u3du(entry)) ||
       (c3n == u3r_cell(entry, &mug, &job)) ||
       (c3n == u3ud(mug)) ||
       (1 < u3r_met(5, mug)) ) {
    u3l_log("pier: load: event %" PRIu64 " malformed.\r\n", id);
    return c3n;
  }

  wit_u->mug_l = u3r_word(0, mug);
  wit_u->job = u3k(job);

  u3z(entry);

  // Insert at queue front since we're loading events in order
  if ( !pir_u->ent_u ) {
    c3_assert(!pir_u->ext_u);

    pir_u->ent_u = pir_u->ext_u = wit_u;
  }
  else {
    if ( wit_u->evt_d != (1ULL + pir_u->ent_u->evt_d) ) {
      fprintf(stderr, "pier: load: commit: event gap: %" PRIx64 ", %"
              PRIx64 "\r\n",
              wit_u->evt_d,
              pir_u->ent_u->evt_d);
      _pier_db_bail(0, "pier: load: comit: event gap");
      return c3n;
    }

    pir_u->ent_u->nex_u = wit_u;
    pir_u->ent_u = wit_u;
  }

  return c3y;
}

/* _pier_db_load_commit(): load len_d commits >= lav_d; enqueue for replay
*/
static void
_pier_db_load_commits(u3_pier* pir_u,
                      c3_d     lav_d,
                      c3_d     len_d)
{
  if (lav_d == 1) {
    // We are restarting from event 1. That means we need to set the ship from
    // the log identity information.
    u3_noun who, fak, len;
    c3_o ret = u3_lmdb_read_identity(pir_u->log_u->db_u,
                                     &who,
                                     &fak,
                                     &len);
    if (ret == c3n) {
      u3l_log("Failed to load identity for replay. Exiting...");
      u3_pier_bail();
    }

    _pier_boot_set_ship(pir_u, u3k(who), u3k(fak));
    pir_u->lif_d = u3r_chub(0, len);

    u3z(who);
    u3z(fak);
    u3z(len);
  }

  c3_o ret = u3_lmdb_read_events(pir_u,
                                 lav_d,
                                 len_d,
                                 _pier_db_on_commit_loaded);
  if (ret == c3n) {
    u3l_log("Failed to read event log for replay. Exiting...");
    u3_pier_bail();
  }
}

/* _pier_db_init():
*/
static c3_o
_pier_db_init(u3_disk* log_u)
{
  c3_d evt_d = 0;
  c3_d pos_d = 0;

  c3_assert( c3n == log_u->liv_o );

  // Request from the database the last event
  if ( c3n == u3_lmdb_get_latest_event_number(log_u->db_u, &evt_d) ) {
    u3l_log("disk init from lmdb failed.");
    return c3n;
  }

  log_u->liv_o = c3y;
  log_u->com_d = log_u->moc_d = evt_d;

  _pier_boot_ready(log_u->pir_u);

  return c3y;
}

/* _pier_disk_create(): load log for given point.
*/
static c3_o
_pier_disk_create(u3_pier* pir_u)
{
  u3_disk* log_u = c3_calloc(sizeof(*log_u));

  pir_u->log_u = log_u;
  log_u->pir_u = pir_u;
  log_u->liv_o = c3n;

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

      // Creates the folder
      if ( 0 == (log_u->com_u = u3_foil_folder(log_c)) ) {
        c3_free(log_c);
        return c3n;
      }

      // Inits the database
      if ( 0 == (log_u->db_u = u3_lmdb_init(log_c)) ) {
        c3_free(log_c);
        return c3n;
      }

      c3_free(log_c);
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

  //  create/load event log
  //
  if ( c3n == _pier_db_init(log_u) ) {
    return c3n;
  }

  return c3y;
}

/* _pier_writ_insert(): insert raw event.
*/
static void
_pier_writ_insert(u3_pier* pir_u,
                  c3_l     msc_l,
                  u3_noun  job)
{
  u3_writ* wit_u = c3_calloc(sizeof(u3_writ));
  wit_u->pir_u = pir_u;

  wit_u->evt_d = pir_u->gen_d;
  pir_u->gen_d++;

  wit_u->msc_l = msc_l;

  wit_u->job = job;

  if ( !pir_u->ent_u ) {
    c3_assert(!pir_u->ext_u);

    pir_u->ent_u = pir_u->ext_u = wit_u;
  }
  else {
    pir_u->ent_u->nex_u = wit_u;
    pir_u->ent_u = wit_u;
  }
}

/* _pier_writ_insert_ovum(): insert raw ovum - for boot sequence.
*/
static void
_pier_writ_insert_ovum(u3_pier* pir_u,
                       c3_l     msc_l,
                       u3_noun  ovo)
{
  u3_noun        now;
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  now = u3_time_in_tv(&tim_tv);

  _pier_writ_insert(pir_u, msc_l, u3nc(now, ovo));
}

/* _pier_writ_find(): find writ by event number.
*/
static u3_writ*
_pier_writ_find(u3_pier* pir_u,
                c3_d     evt_d)
{
  u3_writ* wit_u;

  /* very unlikely to be O(n) and n is small
  */
  for ( wit_u = pir_u->ext_u; wit_u; wit_u = wit_u->nex_u ) {
    if ( evt_d == wit_u->evt_d ) {
      return wit_u;
    }
  }
  return 0;
}

/* _pier_writ_unlink(): unlink writ from queue.
*/
static void
_pier_writ_unlink(u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;

#ifdef VERBOSE_EVENTS
  fprintf(stderr, "pier: (%" PRIu64 "): delete\r\n", wit_u->evt_d);
#endif

  pir_u->ext_u = wit_u->nex_u;

  if ( wit_u == pir_u->ent_u ) {
    c3_assert(pir_u->ext_u == 0);
    pir_u->ent_u = 0;
  }
}

/* _pier_writ_dispose(): dispose of writ.
*/
static void
_pier_writ_dispose(u3_writ* wit_u)
{
  /* free contents
  */
  u3z(wit_u->job);
  u3z(wit_u->mat);
  u3z(wit_u->act);

  c3_free(wit_u);
}

/* _pier_work_bail(): handle subprocess error.
*/
static void
_pier_work_bail(void*       vod_p,
                const c3_c* err_c)
{
  fprintf(stderr, "pier: work error: %s\r\n", err_c);
}

/* _pier_work_boot(): prepare for boot.
*/
static void
_pier_work_boot(u3_pier* pir_u, c3_o sav_o)
{
  u3_controller* god_u = pir_u->god_u;

  c3_assert( 0 != pir_u->lif_d );

  u3_noun who = u3i_chubs(2, pir_u->who_d);
  u3_noun len = u3i_chubs(1, &pir_u->lif_d);

  if ( c3y == sav_o ) {
    _pier_db_write_header(pir_u, who, u3k(pir_u->fak_o), len);
  }

  u3_noun msg = u3nq(c3__boot, who, pir_u->fak_o, len);
  u3_atom mat = u3ke_jam(msg);
  u3_newt_write(&god_u->inn_u, mat, 0);
}

/* _pier_work_shutdown(): stop the worker process.
*/
static void
_pier_work_shutdown(u3_pier* pir_u)
{
  u3_controller* god_u = pir_u->god_u;

  u3_newt_write(&god_u->inn_u, u3ke_jam(u3nc(c3__exit, 0)), 0);
}

/* _pier_work_build(): build atomic action.
*/
static void
_pier_work_build(u3_writ* wit_u)
{
  /* marshal into atom
  */
  if ( 0 == wit_u->mat ) {
    c3_assert(0 != wit_u->job);

    wit_u->mat = u3ke_jam(u3nc(wit_u->mug_l,
                               u3k(wit_u->job)));
  }
}

/* _pier_work_send(): send to worker.
*/
static void
_pier_work_send(u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_controller* god_u = pir_u->god_u;

  c3_assert(0 != wit_u->mat);

  u3_noun msg = u3ke_jam(u3nt(c3__work,
                              u3i_chubs(1, &wit_u->evt_d),
                              u3k(wit_u->mat)));

  u3_newt_write(&god_u->inn_u, msg, wit_u);
}

/* _pier_work_save(): tell worker to save checkpoint.
*/
static void
_pier_work_save(u3_pier* pir_u)
{
  u3_controller* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;
  u3_save* sav_u = pir_u->sav_u;

  c3_assert( god_u->dun_d == sav_u->req_d );
  c3_assert( log_u->com_d >= god_u->dun_d );

  {
    u3_noun mat = u3ke_jam(u3nc(c3__save, u3i_chubs(1, &god_u->dun_d)));
    u3_newt_write(&god_u->inn_u, mat, 0);

    //  XX wait on some report of success before updating?
    //
    sav_u->dun_d = sav_u->req_d;
  }

  //  if we're gracefully shutting down, do so now
  //
  if ( u3_psat_done == pir_u->sat_e ) {
    _pier_exit_done(pir_u);
  }
}

/* _pier_work_release(): apply side effects.
*/
static void
_pier_work_release(u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_controller* god_u = pir_u->god_u;
  u3_noun    vir = wit_u->act;

  if ( u3_psat_pace == pir_u->sat_e ) {
    fputc('.', stderr);

    //  enqueue another batch of events for replay
    //
    {
      u3_disk* log_u = pir_u->log_u;

      //  XX requires that writs be unlinked before effects are released
      //
      if ( (0 == pir_u->ent_u) &&
           (wit_u->evt_d < log_u->com_d) )
      {
        _pier_db_load_commits(pir_u, (1ULL + god_u->dun_d), 1000ULL);
      }
    }
  }
  else {
#ifdef VERBOSE_EVENTS
    fprintf(stderr, "pier: (%" PRIu64 "): compute: release\r\n", wit_u->evt_d);
#endif

    //  advance release counter
    //
    {
      c3_assert(wit_u->evt_d == (1ULL + god_u->rel_d));
      god_u->rel_d += 1ULL;
    }

    //  apply actions
    //
    while ( u3_nul != vir ) {
      u3_noun ovo, nex;
      u3x_cell(vir, &ovo, &nex);

      u3_reck_kick(pir_u, u3k(ovo));
      vir = nex;
    }
  }

  //  if we have completed the boot sequence, activate system events.
  //
  if ( wit_u->evt_d == pir_u->but_d ) {
    _pier_boot_complete(pir_u);
  }

  //  take snapshot, if requested (and awaiting the commit of this event)
  //
  {
    u3_save* sav_u = pir_u->sav_u;

    if ( (sav_u->req_d > sav_u->dun_d) &&
         (wit_u->evt_d == sav_u->req_d) )
    {
      _pier_work_save(pir_u);
    }
  }
}

/* _pier_work_complete(): worker reported completion.
*/
static void
_pier_work_complete(u3_writ* wit_u,
                    c3_l     mug_l,
                    u3_noun  act)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_controller* god_u = pir_u->god_u;

#ifdef VERBOSE_EVENTS
  fprintf(stderr, "pier: (%" PRIu64 "): compute: complete\r\n", wit_u->evt_d);
#endif

  if ( u3_psat_pace == pir_u->sat_e &&
       wit_u->nex_u &&
       mug_l != wit_u->nex_u->mug_l ) {
    // While we are replaying the event log, we also perform checks that the
    // resulting mug_l for this urbit's state is equivalent to the expected
    // input state of the next event. If it isn't, we have either corruption or
    // non-determinism during replay and either should cause a bail.
    u3l_log("Invalid recomputed state. For event %" PRIu64 ", the computed mug "
            "was %x but event %" PRIu64 " expected %x.\r\n",
            wit_u->evt_d, mug_l, wit_u->nex_u->evt_d, wit_u->nex_u->mug_l);

    u3_pier_bail();
  }

  god_u->dun_d += 1;
  c3_assert(god_u->dun_d == wit_u->evt_d);

  god_u->mug_l = mug_l;

  c3_assert(wit_u->act == 0);
  wit_u->act = act;

  if ( wit_u->evt_d > pir_u->lif_d ) {
    u3_term_stop_spinner();
  }
}

/* _pier_work_replace(): worker reported replacement.
*/
static void
_pier_work_replace(u3_writ* wit_u,
                   u3_noun  job)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_controller* god_u = pir_u->god_u;

#ifdef VERBOSE_EVENTS
  fprintf(stderr, "pier: (%" PRIu64 "): compute: replace\r\n", wit_u->evt_d);
#endif

  c3_assert(god_u->sen_d == wit_u->evt_d);

  /* move backward in work processing
  */
  {
    u3z(wit_u->job);
    wit_u->job = job;

    u3z(wit_u->mat);
    wit_u->mat = u3ke_jam(u3nc(wit_u->mug_l,
                               u3k(wit_u->job)));

    god_u->sen_d -= 1;
  }

  if ( wit_u->evt_d > pir_u->lif_d ) {
    u3_term_stop_spinner();
  }
}

/* _pier_work_compute(): dispatch for processing.
*/
static void
_pier_work_compute(u3_writ* wit_u)
{
  u3_pier* pir_u = wit_u->pir_u;
  u3_controller* god_u = pir_u->god_u;

#ifdef VERBOSE_EVENTS
  fprintf(stderr, "pier: (%" PRIu64 "): compute: request\r\n", wit_u->evt_d);
#endif

  c3_assert(wit_u->evt_d == (1 + god_u->sen_d));

  wit_u->mug_l = god_u->mug_l;

  _pier_work_build(wit_u);
  _pier_work_send(wit_u);

  god_u->sen_d += 1;

  if ( wit_u->evt_d > pir_u->lif_d ) {
    u3_term_start_spinner(wit_u->job);
  }
}

/* _pier_work_play(): with active worker, create or load log.
*/
static void
_pier_work_play(u3_pier* pir_u,
                c3_d     lav_d,
                c3_l     mug_l)
{
  u3_controller* god_u = pir_u->god_u;

#ifdef VERBOSE_EVENTS
  fprintf(stderr, "pier: (%" PRIu64 "): boot at mug %x\r\n", lav_d, mug_l);
#endif

  c3_assert( c3n == god_u->liv_o );
  god_u->liv_o = c3y;

  //  all events in the worker are complete
  //
  god_u->rel_d = god_u->dun_d = god_u->sen_d = (lav_d - 1ULL);

  _pier_boot_ready(pir_u);
}

/* _pier_work_stdr(): prints an error message to stderr
 */
static void
_pier_work_stdr(u3_writ* wit_u, u3_noun cord)
{
  c3_c* str = u3r_string(cord);
  u3C.stderr_log_f(str);
  free(str);
}

/* _pier_work_slog(): print directly.
*/
static void
_pier_work_slog(u3_writ* wit_u, c3_w pri_w, u3_noun tan)
{
#ifdef U3_EVENT_TIME_DEBUG
  {
    static int old;
    static struct timeval b4, f2, d0;
    static c3_d b4_d;
    c3_w ms_w;

    if ( old ) {
      gettimeofday(&f2, 0);
      timersub(&f2, &b4, &d0);
      ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
      if (ms_w > 1) {
  #if 0
        fprintf(stderr, "%6d.%02dms: %9d ",
                ms_w, (int) (d0.tv_usec % 1000) / 10,
                ((int) (u3R->pro.nox_d - b4_d)));
  #else
        fprintf(stderr, "%6d.%02dms ",
                ms_w, (int) (d0.tv_usec % 1000) / 10);
  #endif
        gettimeofday(&b4, 0);
        b4_d = u3R->pro.nox_d;
      }
      else {
        fprintf(stderr, "            ");
      }
    }
    else {
      gettimeofday(&b4, 0);
      b4_d = u3R->pro.nox_d;
    }
    old = 1;
  }
#endif

  switch ( pri_w ) {
    case 3: fprintf(stderr, ">>> "); break;
    case 2: fprintf(stderr, ">> "); break;
    case 1: fprintf(stderr, "> "); break;
  }

  u3_pier_tank(0, tan);
}

/* _pier_work_exit(): handle subprocess exit.
*/
static void
_pier_work_exit(uv_process_t* req_u,
                c3_ds         sas_i,
                c3_i          sig_i)
{
  u3_controller* god_u = (void *) req_u;
  u3_pier* pir_u = god_u->pir_u;

  u3l_log("pier: exit: status %" PRIu64 ", signal %d\r\n", sas_i, sig_i);
  uv_close((uv_handle_t*) req_u, 0);

  _pier_db_shutdown(pir_u);
  _pier_work_shutdown(pir_u);
}

/* _pier_work_poke(): handle subprocess result.  transfer nouns.
*/
static void
_pier_work_poke(void*   vod_p,
                u3_noun mat)
{
  u3_pier* pir_u = vod_p;
  u3_noun  jar   = u3ke_cue(u3k(mat));
  u3_noun  p_jar, q_jar, r_jar;

  if ( c3y != u3du(jar) ) {
    goto error;
  }

  switch ( u3h(jar) ) {
    default: goto error;

    //  the worker process starts with a %play task,
    //  which tells us where to start playback
    //  (and who we are, if it knows) XX remove in favor of event-log header
    //
    case c3__play: {
      c3_d lav_d;
      c3_l mug_l;

      if ( (c3n == u3r_qual(u3t(jar), 0, &p_jar, &q_jar, &r_jar)) ||
           (c3n == u3ud(p_jar)) ||
           (u3r_met(6, p_jar) != 1) ||
           (c3n == u3ud(q_jar)) ||
           (u3r_met(5, p_jar) != 1) ||
           (c3n == u3du(r_jar)) ||
           (c3n == u3ud(u3h(r_jar))) ||
           ((c3y != u3t(r_jar)) && (c3n != u3t(r_jar))) )
      {
        if ( u3_nul == u3t(jar) ) {
          lav_d = 1ULL;
          mug_l = 0;
        }
        else {
          goto error;
        }
      }

      if ( u3_nul != u3t(jar) ) {
        lav_d = u3r_chub(0, p_jar);
        mug_l = u3r_word(0, q_jar);

        //  single-home
        //
        _pier_boot_set_ship(pir_u, u3k(u3h(r_jar)), u3k(u3t(r_jar)));
      }

      _pier_work_play(pir_u, lav_d, mug_l);
      break;
    }

    case c3__work: {
      if ( (c3n == u3r_trel(jar, 0, &p_jar, &q_jar)) ||
           (c3n == u3ud(p_jar)) ||
           (u3r_met(6, p_jar) != 1) )
      {
        u3l_log("failed to parse replacement atom");
        goto error;
      }
      else {
        // TODO: This new code is really scary and I don't know how to test it yet.
        //
        c3_d     evt_d = u3r_chub(0, p_jar);
        u3_writ* wit_u = _pier_writ_find(pir_u, evt_d);

        u3_noun mug, job;
        u3_noun entry = u3ke_cue(u3k(q_jar));
        if ( (c3y != u3du(entry)) ||
             (c3n == u3r_cell(entry, &mug, &job)) ||
             (c3n == u3ud(mug)) ||
             (1 < u3r_met(5, mug)) ) {
          goto error;
        }

        c3_l     mug_l = u3r_word(0, mug);
        if ( !wit_u || (mug_l && (mug_l != wit_u->mug_l)) ) {
          goto error;
        }
#ifdef VERBOSE_EVENTS
        fprintf(stderr, "pier: replace: %" PRIu64 "\r\n", evt_d);
#endif

        _pier_work_replace(wit_u, u3k(job));
      }
      break;
    }

    case c3__done: {
      if ( (c3n == u3r_qual(jar, 0, &p_jar, &q_jar, &r_jar)) ||
           (c3n == u3ud(p_jar)) ||
           (u3r_met(6, p_jar) != 1) ||
           (c3n == u3ud(q_jar)) ||
           (u3r_met(5, q_jar) > 1) )
      {
        goto error;
      }
      else {
        c3_d     evt_d = u3r_chub(0, p_jar);
        c3_l     mug_l = u3r_word(0, q_jar);
        u3_writ* wit_u = _pier_writ_find(pir_u, evt_d);

        if ( !wit_u ) {
          u3l_log("poke: no writ: %" PRIu64 "\r\n", evt_d);
          goto error;
        }
        _pier_work_complete(wit_u, mug_l, u3k(r_jar));
      }
      break;
    }

    case c3__stdr: {
      if ( (c3n == u3r_trel(jar, 0, &p_jar, &q_jar)) ||
           (c3n == u3ud(p_jar)) ||
           (u3r_met(6, p_jar) > 1) ||
           (c3n == u3ud(q_jar)) )
      {
        goto error;
      }
      else {
        c3_d     evt_d = u3r_chub(0, p_jar);
        u3_writ* wit_u = _pier_writ_find(pir_u, evt_d);

        // Unlike slog, we always reprint interpreter errors during replay.
        _pier_work_stdr(wit_u, q_jar);
      }
      break;
    }

    case  c3__slog: {
      if ( (c3n == u3r_qual(jar, 0, &p_jar, &q_jar, &r_jar)) ||
           (c3n == u3ud(p_jar)) ||
           (u3r_met(6, p_jar) != 1) ||
           (c3n == u3ud(q_jar)) ||
           (u3r_met(3, q_jar) > 1) )
      {
        goto error;
      }
      else {
        // XXX: The wit_u pointer will almost always be 0 because of how the
        // worker process manages the difference between u3V.evt_d vs
        // u3A->ent_d. Either stop communicating the evt_d in the wire protocol
        // or fix the worker to keep track of and communicate the correct event
        // number.
        c3_d     evt_d = u3r_chub(0, p_jar);
        c3_w     pri_w = u3r_word(0, q_jar);
        u3_writ* wit_u = _pier_writ_find(pir_u, evt_d);

        // Only print this slog if the event is uncommitted.
        if ( u3_psat_pace != pir_u->sat_e ) {
          _pier_work_slog(wit_u, pri_w, u3k(r_jar));
        }
      }
      break;
    }
  }

  u3z(jar); u3z(mat);
  _pier_apply(pir_u);
  return;

  error: {
    u3z(jar); u3z(mat);
    _pier_work_bail(0, "bad jar");
  }
}

/* pier_work_create(): instantiate child process.
*/
static u3_controller*
_pier_work_create(u3_pier* pir_u)
{
  u3_controller* god_u = c3_calloc(sizeof *god_u);

  pir_u->god_u = god_u;
  god_u->pir_u = pir_u;
  god_u->liv_o = c3n;

  /* spawn new process and connect to it
  */
  {
    c3_c* arg_c[5];
    c3_c* bin_c = u3_Host.wrk_c;
    c3_c* pax_c;
    c3_c  key_c[256];
    c3_c  wag_c[11];
    c3_i  err_i;

    pax_c = c3_malloc(1 + strlen(pir_u->pax_c));
    strcpy(pax_c, pir_u->pax_c);

    sprintf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
                   pir_u->key_d[0],
                   pir_u->key_d[1],
                   pir_u->key_d[2],
                   pir_u->key_d[3]);

    sprintf(wag_c, "%u", pir_u->wag_w);

    arg_c[0] = bin_c;                   //  executable
    arg_c[1] = pax_c;                   //  path to checkpoint directory
    arg_c[2] = key_c;                   //  disk key
    arg_c[3] = wag_c;                   //  runtime config
    arg_c[4] = 0;

    uv_pipe_init(u3L, &god_u->inn_u.pyp_u, 0);
    uv_pipe_init(u3L, &god_u->out_u.pyp_u, 0);

    god_u->cod_u[0].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
    god_u->cod_u[0].data.stream = (uv_stream_t *)&god_u->inn_u;

    god_u->cod_u[1].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
    god_u->cod_u[1].data.stream = (uv_stream_t *)&god_u->out_u;

    god_u->cod_u[2].flags = UV_INHERIT_FD;
    god_u->cod_u[2].data.fd = 2;

    god_u->ops_u.stdio = god_u->cod_u;
    god_u->ops_u.stdio_count = 3;

    god_u->ops_u.exit_cb = _pier_work_exit;
    god_u->ops_u.file = arg_c[0];
    god_u->ops_u.args = arg_c;

    if ( (err_i = uv_spawn(u3L, &god_u->cub_u, &god_u->ops_u)) ) {
      fprintf(stderr, "spawn: %s: %s\r\n", arg_c[0], uv_strerror(err_i));

      return 0;
    }
  }

  /* start reading from proc
  */
  {
    god_u->out_u.vod_p = pir_u;
    god_u->out_u.pok_f = _pier_work_poke;
    god_u->out_u.bal_f = _pier_work_bail;

    god_u->inn_u.bal_f = _pier_work_bail;

    u3_newt_read(&god_u->out_u);
  }
  return god_u;
}

/* _pier_loop_time(): set time.
*/
static void
_pier_loop_time(void)
{
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  u3v_time(u3_time_in_tv(&tim_tv));
}

/* _pier_loop_prepare():
*/
static void
_pier_loop_prepare(uv_prepare_t* pep_u)
{
  _pier_loop_time();
}

/* _pier_loop_init_pier(): initialize loop handlers.
*/
static void
_pier_loop_init(u3_pier* pir_u)
{
  c3_l cod_l;

  _pier_loop_time();

  //  for i/o drivers that still use u3A->sen
  //
  u3v_numb();

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_init(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_init(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_init(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__save);
  u3_save_io_init(pir_u);
  u3a_lop(cod_l);

  //  XX legacy handlers, not yet scoped to a pier
  //
  {
    cod_l = u3a_lush(c3__term);
    u3_term_io_init();
    u3a_lop(cod_l);

    cod_l = u3a_lush(c3__http);
    u3_http_io_init();
    u3a_lop(cod_l);

    cod_l = u3a_lush(c3__cttp);
    u3_cttp_io_init();
    u3a_lop(cod_l);
  }
}

/* _pier_loop_wake(): initialize listeners and send initial events.
*/
static void
_pier_loop_wake(u3_pier* pir_u)
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_talk(pir_u);
  u3_unix_ef_bake(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_talk(pir_u);
  u3_ames_ef_bake(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_ef_bake(pir_u);
  u3a_lop(cod_l);

  //  XX legacy handlers, not yet scoped to a pier
  //
  {
    cod_l = u3a_lush(c3__http);
    u3_http_io_talk();
    u3_http_ef_bake();
    u3a_lop(cod_l);

    cod_l = u3a_lush(c3__term);
    u3_term_io_talk();
    u3_term_ef_bake();
    u3a_lop(cod_l);
  }
}

/* _pier_loop_exit(): terminate I/O across the process.
*/
static void
_pier_loop_exit(u3_pier* pir_u)
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_exit(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_exit(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__save);
  u3_save_io_exit(pir_u);
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_exit(pir_u);
  u3a_lop(cod_l);

  //  XX legacy handlers, not yet scoped to a pier
  //
  {
    cod_l = u3a_lush(c3__term);
    u3_term_io_exit();
    u3a_lop(cod_l);

    cod_l = u3a_lush(c3__http);
    u3_http_io_exit();
    u3a_lop(cod_l);

    cod_l = u3a_lush(c3__cttp);
    u3_cttp_io_exit();
    u3a_lop(cod_l);
  }
}

/* _pier_boot_set_ship():
*/
static void
_pier_boot_set_ship(u3_pier* pir_u, u3_noun who, u3_noun fak)
{
  c3_assert( c3y == u3ud(who) );
  c3_assert( (c3y == fak) || (c3n == fak) );

  c3_o fak_o = fak;
  c3_d who_d[2];

  u3r_chubs(0, 2, who_d, who);

  c3_assert( ( (0 == pir_u->fak_o) &&
               (0 == pir_u->who_d[0]) &&
               (0 == pir_u->who_d[1]) ) ||
             ( (fak_o == pir_u->fak_o) &&
               (who_d[0] == pir_u->who_d[0]) &&
               (who_d[1] == pir_u->who_d[1]) ) );

  pir_u->fak_o = fak_o;
  pir_u->who_d[0] = who_d[0];
  pir_u->who_d[1] = who_d[1];

  {
    u3_noun how = u3dc("scot", 'p', u3k(who));

    c3_free(pir_u->who_c);
    pir_u->who_c = u3r_string(how);
    u3z(how);
  }

  //  Disable networking for fake ships
  //
  if ( c3y == pir_u->fak_o ) {
    u3_Host.ops_u.net = c3n;
  }

  u3z(who); u3z(fak);
}

/* _pier_boot_create(): create boot controller
*/
static u3_boot*
_pier_boot_create(u3_pier* pir_u, u3_noun pil, u3_noun ven)
{
  u3_boot* bot_u = c3_calloc(sizeof(u3_boot));
  bot_u->pil = u3k(pil);
  bot_u->ven = u3k(ven);
  bot_u->pir_u = pir_u;

  return bot_u;
}

/* _pier_boot_dispose(): dispose of boot controller
*/
static void
_pier_boot_dispose(u3_boot* bot_u)
{
  u3_pier* pir_u = bot_u->pir_u;

  u3z(bot_u->pil);
  u3z(bot_u->ven);
  free(bot_u);
  pir_u->bot_u = 0;
}

/* _pier_boot_vent(): create and enqueue boot sequence
**
**  per cgy:
**    this new boot sequence is almost, but not quite,
**    the right thing.  see new arvo.
*/
static void
_pier_boot_vent(u3_boot* bot_u)
{
  //  bot: boot formulas
  //  mod: module ova
  //  use: userpace ova
  //
  u3_noun bot, mod, use;
  u3_pier* pir_u = bot_u->pir_u;

  //  extract boot formulas and module/userspace ova from pill
  //
  {
    u3_noun pil_p, pil_q, pil_r;
    u3_noun pro;

    c3_assert( c3y == u3du(bot_u->pil) );

    if ( c3y == u3h(bot_u->pil) ) {
      u3x_trel(bot_u->pil, 0, &pil_p, &pil_q);
    }
    else {
      u3x_qual(bot_u->pil, 0, &pil_p, &pil_q, &pil_r);
    }

    pro = u3m_soft(0, u3ke_cue, u3k(pil_p));

    if ( 0 != u3h(pro) ) {
      fprintf(stderr, "boot: failed: unable to parse pill\r\n");
      exit(1);
    }

    u3x_trel(u3t(pro), &bot, &mod, &use);
    u3k(bot); u3k(mod); u3k(use);

    //  optionally replace filesystem in userspace
    //
    if ( c3y == u3h(bot_u->pil) ) {
      if ( u3_nul != pil_q ) {
        c3_w len_w = 0;
        u3_noun ova = use;
        u3_noun new = u3_nul;
        u3_noun ovo;

        while ( u3_nul != ova ) {
          ovo = u3h(ova);

          if ( c3__into == u3h(u3t(ovo)) ) {
            c3_assert( 0 == len_w );
            len_w++;
            ovo = u3k(u3t(pil_q));
          }

          new = u3nc(u3k(ovo), new);
          ova = u3t(ova);
        }

        c3_assert( 1 == len_w );

        u3z(use);
        use = u3kb_flop(new);
      }
    }
    //  prepend %lite module and userspace ova
    //
    else {
      mod = u3kb_weld(u3k(pil_q), mod);
      use = u3kb_weld(u3k(pil_r), use);
    }

    u3z(pro);
  }

  //  prepend entropy to the module sequence
  //
  //    XX also copy to _pier_loop_wake?
  //
  {
    c3_w    eny_w[16];
    c3_rand(eny_w);

    u3_noun wir = u3nt(u3_blip, c3__arvo, u3_nul);
    u3_noun car = u3nc(c3__wack, u3i_words(16, eny_w));

    mod = u3nc(u3nc(wir, car), mod);
  }

  //  prepend identity to the module sequence, setting single-home
  //
  {
    u3_noun wir = u3nt(u3_blip, c3__arvo, u3_nul);
    u3_noun car = u3nc(c3__whom, u3i_chubs(2, pir_u->who_d));

    mod = u3nc(u3nc(wir, car), mod);
  }

  //  insert boot sequence directly
  //
  //    Note that these are not ovum or (pair @da ovum) events,
  //    but raw nock formulas to be directly evaluated as the
  //    subject of the lifecycle formula [%2 [%0 3] %0 2].
  //    All subsequent events will be (pair @da ovum).
  //
  {
    u3_noun fol = bot;

    //  initialize the boot barrier
    //
    //    And the initial lifecycle boot barrier.
    //
    pir_u->but_d = u3kb_lent(u3k(fol));
    pir_u->lif_d = pir_u->but_d;

    while ( u3_nul != fol ) {
      _pier_writ_insert(pir_u, 0, u3k(u3h(fol)));
      fol = u3t(fol);
    }
  }

  //  insert module events
  //
  {
    u3_noun ova = mod;
    //  add to the boot barrier
    //
    pir_u->but_d += u3kb_lent(u3k(ova));

    while ( u3_nul != ova ) {
      _pier_writ_insert_ovum(pir_u, 0, u3k(u3h(ova)));
      ova = u3t(ova);
    }
  }

  //  insert legacy boot event
  //
  {
    //  XX do something about this wire
    //  XX route directly to %jael?
    //
    c3_assert( c3y == u3du(bot_u->ven) );

    u3_noun wir = u3nq(u3_blip, c3__term, '1', u3_nul);
    u3_noun car = u3nc(c3__boot, u3k(bot_u->ven));
    u3_noun ovo = u3nc(wir, car);

    _pier_writ_insert_ovum(pir_u, 0, ovo);
  }

  //  insert userspace events
  //
  //    Currently just the initial filesystem
  //
  {
    u3_noun ova = use;

    while ( u3_nul != ova ) {
      _pier_writ_insert_ovum(pir_u, 0, u3k(u3h(ova)));
      ova = u3t(ova);
    }
  }

  u3z(bot); u3z(mod); u3z(use);
}

/* _pier_boot_complete(): start organic event flow on boot/reboot.
*/
static void
_pier_boot_complete(u3_pier* pir_u)
{
  if ( u3_psat_init != pir_u->sat_e ) {
    u3_pier_snap(pir_u);
  }

  if ( u3_psat_boot == pir_u->sat_e ) {
    fprintf(stderr, "pier: boot complete\r\n");
  }
  else if ( u3_psat_pace == pir_u->sat_e ) {
    fprintf(stderr, "\n\r---------------- playback complete----------------\r\n");
  }

  pir_u->sat_e = u3_psat_play;

  //  the main course
  //
  _pier_loop_wake(pir_u);

  //  XX where should this go?
  //
  {
    if ( c3y == u3_Host.ops_u.veb ) {
      u3_term_ef_verb();
    }
  }
}

/* _pier_boot_ready():
*/
static void
_pier_boot_ready(u3_pier* pir_u)
{
  u3_controller* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;

  c3_assert( u3_psat_init == pir_u->sat_e );

  if ( ( 0 == god_u) ||
       ( 0 == log_u) ||
       (c3y != god_u->liv_o) ||
       (c3y != log_u->liv_o) )
  {
    return;
  }

  //  mark all commits as released
  //
  god_u->rel_d = log_u->com_d;

  //  set next expected event number
  //
  pir_u->gen_d = (1ULL + log_u->com_d);

  //  boot
  //
  if ( 0 != pir_u->bot_u ) {
    c3_assert( 0 == log_u->com_d );
    c3_assert( 0 == god_u->dun_d );

    //  construct/enqueue boot sequence
    //
    _pier_boot_vent(pir_u->bot_u);
    _pier_boot_dispose(pir_u->bot_u);

    //  prepare worker for boot sequence, write log header
    //
    _pier_work_boot(pir_u, c3y);

    fprintf(stderr, "boot: ship: %s%s\r\n",
                     pir_u->who_c,
                     (c3y == pir_u->fak_o) ? " (fake)" : "");

    pir_u->sat_e = u3_psat_boot;
  }
  //  replay
  //
  else if ( god_u->dun_d < log_u->com_d ) {
    c3_assert( 0 != log_u->com_d );

    fprintf(stderr, "---------------- playback starting----------------\r\n");

    //  set the boot barrier to the last committed event
    //
    pir_u->but_d = log_u->com_d;

    //  begin queuing batches of committed events
    //
    _pier_db_load_commits(pir_u, (1ULL + god_u->dun_d), 1000ULL);

    if ( 0 == god_u->dun_d ) {
      fprintf(stderr, "pier: replaying events 1 through %" PRIu64 "\r\n",
                      log_u->com_d);

      //  prepare worker for replay of boot sequence, don't write log header
      //
      _pier_work_boot(pir_u, c3n);
    }
    else {
      fprintf(stderr, "pier: replaying events %" PRIu64
                      " through %" PRIu64 "\r\n",
                      god_u->dun_d,
                      log_u->com_d);
    }

    pir_u->sat_e = u3_psat_pace;
  }
  //  resume
  //
  else {
    c3_assert( 0 != log_u->com_d );
    c3_assert( 0 != god_u->dun_d );

    //  set the boot barrier to the last computed event
    //
    pir_u->but_d = god_u->dun_d;

    //  resume normal operation
    //
    _pier_boot_complete(pir_u);
  }
}

/* _pier_apply(): react to i/o, inbound or outbound.
*/
static void
_pier_apply(u3_pier* pir_u)
{
  u3_disk* log_u = pir_u->log_u;
  u3_controller* god_u = pir_u->god_u;
  u3_save* sav_u = pir_u->sav_u;

  if ( (0 == log_u) ||
       (0 == god_u) ||
       (c3n == god_u->liv_o) ||
       (u3_psat_init == pir_u->sat_e) )
  {
    return;
  }

  u3_writ* wit_u;
  c3_o     act_o = c3n;

start:

  /* iterate from queue exit, advancing any writs that can advance
  */
  wit_u = pir_u->ext_u;
  while ( wit_u ) {
    /* if writ is (a) next in line to compute, (b) worker is inactive,
    ** and (c) a snapshot has not been requested, request computation
    */
    if ( (wit_u->evt_d == (1 + god_u->sen_d)) &&
         (god_u->sen_d == god_u->dun_d) &&
         (sav_u->dun_d == sav_u->req_d) )
    {
      _pier_work_compute(wit_u);
      act_o = c3y;
    }

    /* if writ is (a) computed and (b) next in line to commit,
    ** and (c) no commit is in progress and (d) we've booted,
    ** request commit.
    */
    if ( (wit_u->evt_d <= god_u->dun_d) &&
         (wit_u->evt_d == (1 + log_u->moc_d)) &&
         (wit_u->evt_d == (1 + log_u->com_d)) )
    {
      // TODO(erg): This is the place where we build up things into a queue.
      _pier_db_commit_request(wit_u);
      act_o = c3y;
    }

    /* if writ is (a) committed and (b) computed,
    ** release effects and delete from queue
    */
    if ( (wit_u->evt_d <= log_u->com_d) &&
         (wit_u->evt_d <= god_u->dun_d) )
    {
      //  effects must be released in order
      //
      c3_assert(wit_u == pir_u->ext_u);

      //  remove from queue
      //
      //    XX must be done before releasing effects
      //    which is currently reentrant
      //
      _pier_writ_unlink(wit_u);

      //  release effects
      //
      _pier_work_release(wit_u);

      //  free writ
      //
      _pier_writ_dispose(wit_u);

      wit_u = pir_u->ext_u;
      act_o = c3y;
    }
    else {
      /* otherwise, continue backward
      */
      wit_u = wit_u->nex_u;
    }
  }

  /* if we did anything to the queue, make another pass.
  */
  if ( c3y == act_o ) {
    act_o = c3n;
    goto start;
  }
}

/* _pier_create(): create a pier, loading existing.
*/
static u3_pier*
_pier_create(c3_w wag_w, c3_c* pax_c)
{
  //  create pier
  //
  u3_pier* pir_u = c3_calloc(sizeof *pir_u);

  pir_u->pax_c = pax_c;
  pir_u->wag_w = wag_w;
  pir_u->sat_e = u3_psat_init;

  pir_u->sam_u = c3_calloc(sizeof(u3_ames));
  pir_u->teh_u = c3_calloc(sizeof(u3_behn));
  pir_u->unx_u = c3_calloc(sizeof(u3_unix));
  pir_u->sav_u = c3_calloc(sizeof(u3_save));

  //  initialize persistence
  //
  if ( c3n == _pier_disk_create(pir_u) ) {
    return 0;
  }

  //  start the worker process
  //
  if ( !(pir_u->god_u = _pier_work_create(pir_u)) ) {
    return 0;
  }

  //  install in the pier table
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

/* u3_pier_interrupt(): interrupt running process.
*/
void
u3_pier_interrupt(u3_pier* pir_u)
{
  uv_process_kill(&pir_u->god_u->cub_u, SIGINT);
}

/* _pier_exit_done(): synchronously shutting down
*/
static void
_pier_exit_done(u3_pier* pir_u)
{
  u3l_log("pier: exit\r\n");

  _pier_db_shutdown(pir_u);
  _pier_work_shutdown(pir_u);
  _pier_loop_exit(pir_u);

  //  XX uninstall pier from u3K.tab_u, dispose

  //  XX no can do
  //
  uv_stop(u3L);
}

/* u3_pier_exit(): trigger a gentle shutdown.
*/
void
u3_pier_exit(u3_pier* pir_u)
{
  pir_u->sat_e = u3_psat_done;

  //  XX must wait for callback confirming
  //
  u3_pier_snap(pir_u);
}

/* u3_pier_snap(): request snapshot
*/
void
u3_pier_snap(u3_pier* pir_u)
{
  u3_controller* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;
  u3_save* sav_u = pir_u->sav_u;

  c3_d top_d = c3_max(god_u->sen_d, god_u->dun_d);

  //  no-op if there are no un-snapshot'ed events
  //
  if ( top_d > sav_u->dun_d ) {
    sav_u->req_d = top_d;

    //  save eagerly if all computed events are already committed
    //
    if ( (log_u->com_d >= top_d) &&
         (god_u->dun_d == top_d) ) {
      _pier_work_save(pir_u);
    }
  }
  //  if we're gracefully shutting down, do so now
  //
  else if ( u3_psat_done == pir_u->sat_e ) {
    _pier_exit_done(pir_u);
  }
}

/* u3_pier_discover(): insert task into process controller.
*/
void
u3_pier_discover(u3_pier* pir_u,
                 c3_l     msc_l,
                 u3_noun  job)
{
  _pier_writ_insert(pir_u, msc_l, job);
  _pier_apply(pir_u);
}

/* u3_pier_send(): modern send with target and path.
*/
void
u3_pier_send(u3_pier* pir_u, u3_noun pax, u3_noun tag, u3_noun fav)
{
}

/* u3_pier_work(): send event; real pier pointer.
**
**    XX: u3_pier_work() is for legacy events sent to a real pier.
*/
void
u3_pier_work(u3_pier* pir_u, u3_noun pax, u3_noun fav)
{
  u3_noun        now;
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  now = u3_time_in_tv(&tim_tv);

  u3_pier_discover(pir_u, 0, u3nt(now, pax, fav));
}

/* u3_pier_plan(): send event; fake pier pointer
**
**    XX: u3_pier_plan() is maximum legacy, do not use.
*/
void
u3_pier_plan(u3_noun pax, u3_noun fav)
{
  u3_pier_work(u3_pier_stub(), pax, fav);
}

/* c3_rand(): fill a 512-bit (16-word) buffer.
*/
void
c3_rand(c3_w* rad_w)
{
  if ( 0 != ent_getentropy(rad_w, 64) ) {
    u3l_log("c3_rand getentropy: %s\n", strerror(errno));
    //  XX review
    //
    u3_pier_bail();
  }
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

/* _pier_tape(): dump a tape, old style.  Don't do this.
*/
static void
_pier_tape(FILE* fil_u, u3_noun tep)
{
  u3_noun tap = tep;

  while ( c3y == u3du(tap) ) {
    c3_c car_c;

    if ( u3h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u3h(tap);

    putc(car_c, fil_u);
    tap = u3t(tap);
  }
  u3z(tep);
}

/* _pier_wall(): dump a wall, old style.  Don't do this.
*/
static void
_pier_wall(u3_noun wol)
{
  FILE* fil_u = u3_term_io_hija();
  u3_noun wal = wol;

  //  XX temporary, for urb.py test runner
  //
  if ( c3y == u3_Host.ops_u.dem ) {
    fil_u = stderr;
  }

  while ( u3_nul != wal ) {
    _pier_tape(fil_u, u3k(u3h(wal)));

    putc(13, fil_u);
    putc(10, fil_u);

    wal = u3t(wal);
  }
  u3_term_io_loja(0);
  u3z(wol);
}

/* u3_pier_tank(): dump single tank.
*/
void
u3_pier_tank(c3_l tab_l, u3_noun tac)
{
  u3_pier_punt(tab_l, u3nc(tac, u3_nul));
}

/* u3_pier_punt(): dump tank list.
*/
void
u3_pier_punt(c3_l tab_l, u3_noun tac)
{
  u3_noun blu   = u3_term_get_blew(0);
  c3_l    col_l = u3h(blu);
  u3_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( c3y == u3r_du(cat) ) {
    if ( 0 == u3A->roc ) {
      u3_noun act = u3h(cat);

      if ( c3__leaf == u3h(act) ) {
        FILE* fil_u = u3_term_io_hija();

        //  XX temporary, for urb.py test runner
        //
        if ( c3y == u3_Host.ops_u.dem ) {
          fil_u = stderr;
        }

        _pier_tape(fil_u, u3k(u3t(act)));
        putc(13, fil_u);
        putc(10, fil_u);

        u3_term_io_loja(0);
      }
    }
    else {
      u3_noun wol = u3dc("wash", u3nc(tab_l, col_l), u3k(u3h(cat)));

      _pier_wall(wol);
    }
    cat = u3t(cat);
  }
  u3z(tac);
  u3z(blu);
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

/* u3_pier_boot(): start the new pier system.
*/
void
u3_pier_boot(c3_w  wag_w,                   //  config flags
             u3_noun who,                   //  identity
             u3_noun ven,                   //  boot event
             u3_noun pil,                   //  type-of/path-to pill
             u3_noun pax)                   //  path to pier
{
  //  make/load pier
  //
  u3_pier* pir_u = _pier_create(wag_w, u3r_string(pax));

  //  set boot params
  //
  {
    pir_u->bot_u = _pier_boot_create(pir_u, u3k(pil), u3k(ven));

    _pier_boot_set_ship(pir_u, u3k(who), ( c3__fake == u3h(ven) ) ? c3y : c3n);
  }

  //  initialize i/o handlers
  //
  _pier_loop_init(pir_u);

  //  initialize polling handle
  //
  uv_prepare_init(u3_Host.lup_u, &pir_u->pep_u);
  uv_prepare_start(&pir_u->pep_u, _pier_loop_prepare);

  u3z(who); u3z(ven); u3z(pil); u3z(pax);
}

/* u3_pier_stay(): resume the new pier system.
*/
void
u3_pier_stay(c3_w wag_w, u3_noun pax)
{
  //  make/load pier
  //
  u3_pier* pir_u = _pier_create(wag_w, u3r_string(pax));

  //  initialize i/o handlers
  //
  _pier_loop_init(pir_u);

  //  initialize polling handle
  //
  uv_prepare_init(u3_Host.lup_u, &pir_u->pep_u);
  uv_prepare_start(&pir_u->pep_u, _pier_loop_prepare);

  u3z(pax);
}

/* u3_pier_mark(): mark all Loom allocations in all u3_pier structs.
*/
c3_w
u3_pier_mark(FILE* fil_u)
{
  c3_w len_w = u3K.len_w;
  c3_w tot_w = 0;
  u3_pier* pir_u;

  while ( 0 < len_w ) {
    pir_u = u3K.tab_u[--len_w];
    u3l_log("pier: %u\r\n", len_w);

    if ( 0 != pir_u->bot_u ) {
      tot_w += u3a_maid(fil_u, "  boot event", u3a_mark_noun(pir_u->bot_u->ven));
      tot_w += u3a_maid(fil_u, "  pill", u3a_mark_noun(pir_u->bot_u->pil));
    }

    {
      u3_writ* wit_u = pir_u->ent_u;
      c3_w wit_w = 0;

      while ( 0 != wit_u ) {
        wit_w += u3a_mark_noun(wit_u->job);
        wit_w += u3a_mark_noun(wit_u->now);
        wit_w += u3a_mark_noun(wit_u->mat);
        wit_w += u3a_mark_noun(wit_u->act);
        wit_u = wit_u->nex_u;
      }

      tot_w += u3a_maid(fil_u, "  writs", wit_w);
    }
  }

  return tot_w;
}
