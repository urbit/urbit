/* vere/disk.c
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
#include <vere/db/lmdb.h>

struct _cd_read {
  c3_d             eve_d;
  c3_d             len_d;
  struct _u3_fact* ent_u;               //  queue entry
  struct _u3_fact* ext_u;               //  queue exit
  struct _u3_disk* log_u;
};

/* u3_db_batch: database write batch
*/
  typedef struct _u3_db_batch {
    c3_d             eve_d;               //  first event
    c3_d             len_d;               //  number of events
    void**           byt_p;               //  array of bytes
    size_t*          siz_i;               //  array of lengths
  } u3_db_batch;

/* _write_request: callback struct for c3_lmdb_write_event()
**
**   Note that [env_u] is thread-safe, but, transactions and handles
**   opened from it are explicitly not. [dun_f] is called on the main thread
**
*/
struct _cd_save {
  c3_o             ret_o;               //  result
  u3_db_batch*     bat_u;               //  write batch
  struct _u3_disk* log_u;
};

#undef VERBOSE_DISK

static void
_disk_commit(u3_disk* log_u);

/* u3_disk_init(): load or create pier and log.
*/
u3_disk*
u3_disk_init(c3_c* pax_c, u3_disk_cb cb_u)
{
  u3_disk* log_u = c3_calloc(sizeof(*log_u));
  log_u->liv_o = c3n;
  log_u->hol_o = c3n;
  log_u->cb_u  = cb_u;

  uv_timer_init(u3L, &log_u->tim_u);

  //  create/load pier directory
  //
  {
    if ( 0 == (log_u->dir_u = u3_foil_folder(pax_c)) ) {
      fprintf(stderr, "disk: failed to load pier at %s", pax_c);
      c3_free(log_u);
      return 0;
    }
  }

  //  create/load $pier/.urb
  //
  {
    c3_c* urb_c = c3_malloc(6 + strlen(pax_c));

    strcpy(urb_c, pax_c);
    strcat(urb_c, "/.urb");

    if ( 0 == (log_u->urb_u = u3_foil_folder(urb_c)) ) {
      fprintf(stderr, "disk: failed to load /.urb in %s", pax_c);
      c3_free(urb_c);
      c3_free(log_u);
      return 0;
    }
    c3_free(urb_c);
  }

  //  create/load $pier/.urb/put and $pier/.urb/get
  //
  {
    c3_c* dir_c = c3_malloc(10 + strlen(pax_c));

    strcpy(dir_c, pax_c);
    strcat(dir_c, "/.urb/put");
    mkdir(dir_c, 0700);

    strcpy(dir_c, pax_c);
    strcat(dir_c, "/.urb/get");
    mkdir(dir_c, 0700);

    c3_free(dir_c);
  }

  //  create/load $pier/.urb/log, initialize db
  //
  {
    c3_c* log_c = c3_malloc(10 + strlen(pax_c));

    strcpy(log_c, pax_c);
    strcat(log_c, "/.urb/log");

    if ( 0 == (log_u->com_u = u3_foil_folder(log_c)) ) {
      fprintf(stderr, "disk: failed to load /.urb/log in %s", pax_c);
      c3_free(log_c);
      c3_free(log_u);
      return 0;
    }

    {
      // TODO: Start with forty gigabytes on macOS and sixty otherwise for the
      // maximum event log size. We'll need to do something more sophisticated for
      // real in the long term, though.
      //
#ifdef U3_OS_osx
      const size_t siz_w = 42949672960;
#else
      const size_t siz_w = 64424509440;;
#endif

      if ( 0 == (log_u->mdb_u = c3_lmdb_init(log_c, siz_w)) ) {
        fprintf(stderr, "disk: failed to initialize database");
        c3_free(log_c);
        c3_free(log_u);
        return 0;
      }
    }

    c3_free(log_c);
  }

  //  get the latest event number from the db
  //
  {
    log_u->dun_d = 0;
    c3_d fir_d;

    if ( c3n == c3_lmdb_gulf(log_u->mdb_u, &fir_d, &log_u->dun_d) ) {
      fprintf(stderr, "disk: failed to load latest event from database");
      c3_free(log_u);
      return 0;
    }

    log_u->sen_d = log_u->dun_d;
  }

  log_u->liv_o = c3y;

  return log_u;
}

static void
_disk_meta_read_cb(void* vod_p, size_t val_i, void* val_p)
{
  u3_weak* mat = vod_p;

  if ( val_p ) {
    *mat = u3i_bytes(val_i, val_p);
  }
}

static u3_weak
_disk_read_meta(u3_disk* log_u, const c3_c* key_c)
{
  u3_weak mat = u3_none;

  c3_lmdb_read_meta(log_u->mdb_u, &mat, "who", _disk_meta_read_cb);

  if ( u3_none == mat ) {
    return u3_none;
  }

  {
    u3_noun pro = u3m_soft(0, u3ke_cue, mat);
    u3_noun tag, dat;
    u3x_cell(pro, &tag, &dat);

    if ( u3_blip == tag ) {
      u3k(dat);
      u3z(pro);
      return dat;
    }
    else {
      fprintf(stderr, "disk: meta cue failed\r\n");
      u3z(pro);
      return u3_none;
    }
  }
}

c3_o
u3_disk_read_header(u3_disk* log_u, c3_d* who_d, c3_o* fak_o, c3_w* lif_w)
{
  u3_weak who = _disk_read_meta(log_u, "who");
  u3_weak fak = _disk_read_meta(log_u, "is-fake");
  u3_weak lif = _disk_read_meta(log_u, "life");

  if ( u3_none == who ) {
    return c3n;
  }
  else if (  (u3_none == fak)
          || (u3_none == lif) )
  {
    u3z(who);
    return c3n;
  }

  if (  (c3n == u3a_is_cat(lif))
     || !((c3y == fak ) || (c3n == fak )) )
  {
    u3z(who); u3z(fak); u3z(lif);
    return c3n;
  }

  if ( who_d ) {
    u3r_chubs(0, 2, who_d, who);
  }

  if ( fak_o ) {
    *fak_o = fak;
  }

  if ( lif_w ) {
    *lif_w = lif;
  }

  u3z(who);
  return c3y;
}

static c3_o
_disk_save_meta(u3_disk* log_u, const c3_c* key_c, u3_atom dat)
{
  u3_atom  mat = u3ke_jam(dat);
  c3_w   len_w = u3r_met(3, mat);
  c3_y*  byt_y = c3_malloc(len_w);
  c3_o   ret_o;

  u3r_bytes(0, len_w, byt_y, mat);

  ret_o = c3_lmdb_save_meta(log_u->mdb_u, key_c, len_w, byt_y);

  u3z(mat);
  c3_free(byt_y);

  return ret_o;
}

c3_o
u3_disk_write_header(u3_disk* log_u, c3_d who_d[2], c3_o fak_o, c3_w lif_w)
{
  c3_assert( c3y == u3a_is_cat(lif_w) );

  if (  (c3n == _disk_save_meta(log_u, "who", u3i_chubs(2, who_d)))
     || (c3n == _disk_save_meta(log_u, "is-fake", fak_o))
     || (c3n == _disk_save_meta(log_u, "life", lif_w)) )
  {
    //  XX dispose?
    //
    return c3n;
  }

  return c3y;
}

static void
_disk_free_batch(u3_db_batch* bat_u)
{
  while ( bat_u->len_d-- ) {
    c3_free(bat_u->byt_p[bat_u->len_d]);
  }

  c3_free(bat_u->byt_p);
  c3_free(bat_u->siz_i);
  c3_free(bat_u);
}

/* _disk_commit_done(): commit complete.
 */
static void
_disk_commit_done(void* vod_p, c3_o ret_o, u3_db_batch* bat_u)
{
  u3_disk* log_u = vod_p;
  c3_d     eve_d = bat_u->eve_d;
  c3_d     len_d = bat_u->len_d;

  if ( c3n == ret_o ) {
    log_u->cb_u.write_bail_f(log_u->cb_u.vod_p, eve_d + (len_d - 1ULL));

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: failed\r\n", eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: failed\r\n",
                      eve_d,
                      eve_d + (len_d - 1ULL));
    }
#endif
  }
  else {
    log_u->dun_d = eve_d + (len_d - 1ULL);
    log_u->cb_u.write_done_f(log_u->cb_u.vod_p, log_u->dun_d);

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: complete\r\n", eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: complete\r\n",
                      eve_d,
                      eve_d + (len_d - 1ULL));
    }
#endif
  }

  {
    u3_fact* tac_u = log_u->put_u.ext_u;
    
    while ( tac_u && (tac_u->eve_d <= log_u->dun_d) ) {
      log_u->put_u.ext_u = tac_u->nex_u;
      u3z(tac_u->job);
      c3_free(tac_u);
      tac_u = log_u->put_u.ext_u;
    }
  }

  if ( !log_u->put_u.ext_u ) {
    log_u->put_u.ent_u = 0;
  }

  _disk_free_batch(bat_u);

  log_u->hol_o = c3n;
  _disk_commit(log_u);
}




/* _disk_commit_after_cb(): Implementation of c3_lmdb_write_event()
**
** This is always run on the main loop thread after the worker thread event
** completes.
*/
static void
_disk_commit_after_cb(uv_work_t* ted_u, int status)
{
  struct _cd_save* req_u = ted_u->data;
  _disk_commit_done(req_u->log_u, req_u->ret_o, req_u->bat_u);
  c3_free(req_u);
  c3_free(ted_u);
}

/* _lmdb_write_event_cb(): Implementation of c3_lmdb_write_event()
**
** This is always run on a libuv background worker thread; actual nouns cannot
** be touched here.
*/
static void
_disk_commit_cb(uv_work_t* ted_u)
{
  struct _cd_save* req_u = ted_u->data;
  u3_db_batch*     bat_u = req_u->bat_u;
  req_u->ret_o = c3_lmdb_save(req_u->log_u->mdb_u,
                              bat_u->eve_d,
                              bat_u->len_d,
                              bat_u->byt_p,
                              bat_u->siz_i);
}

/* c3_lmdb_write_event(): Asynchronously writes events to the database.
**
** This writes all the passed in events along with log metadata updates to the
** database as a single transaction on a worker thread. Once the transaction
** is completed, it calls the passed in callback on the main loop thread.
*/
static void
_disk_commit_start(u3_disk* log_u, u3_db_batch* bat_u)
{
  //  structure to pass to the worker thread.
  //
  struct _cd_save* req_u = c3_malloc(sizeof(*req_u));
  req_u->log_u = log_u;
  req_u->bat_u = bat_u;
  req_u->ret_o = c3n;

  //  queue asynchronous work to happen on another thread
  //
  uv_work_t* ted_u = c3_malloc(sizeof(*ted_u));
  ted_u->data = req_u;

  uv_queue_work(u3L, ted_u, _disk_commit_cb,
                            _disk_commit_after_cb);
}

static void
_disk_commit(u3_disk* log_u)
{
  if (  (c3n == log_u->hol_o)
     && (log_u->sen_d > log_u->dun_d) )
  {
    c3_d len_d = log_u->sen_d - log_u->dun_d;
    u3_fact* tac_u = log_u->put_u.ext_u;

    c3_assert( (1ULL + log_u->dun_d) == tac_u->eve_d );
    c3_assert( log_u->sen_d == log_u->put_u.ent_u->eve_d );

    u3_db_batch* bat_u = c3_malloc(sizeof(*bat_u));
    bat_u->eve_d = tac_u->eve_d;
    bat_u->len_d = len_d;
    bat_u->byt_p = c3_malloc(len_d * sizeof(void*));
    bat_u->siz_i = c3_malloc(len_d * sizeof(size_t));

    for ( c3_d i_d = 0ULL; i_d < len_d; ++i_d) {
      c3_assert( (bat_u->eve_d + i_d) == tac_u->eve_d );

      u3_atom mat = u3ke_jam(u3nc(tac_u->bug_l, u3k(tac_u->job)));
      c3_w  len_w = u3r_met(3, mat);
      c3_y* dat_y = c3_malloc(len_w);
      u3r_bytes(0, len_w, dat_y, mat);

      bat_u->byt_p[i_d] = dat_y;
      bat_u->siz_i[i_d] = len_w;

      tac_u = tac_u->nex_u;
      u3z(mat);
    }

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: request\r\n",
                      bat_u->eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: request\r\n",
                      bat_u->eve_d,
                      (bat_u->eve_d + len_d - 1ULL));
    }
#endif

    _disk_commit_start(log_u, bat_u);
    log_u->hol_o = c3y;
  }
}

/* u3_disk_plan():
*/
void
u3_disk_plan(u3_disk* log_u,
             c3_d     eve_d,
             c3_l     bug_l,
             c3_l     mug_l,
             u3_noun  job)
{
  u3_fact* tac_u = c3_malloc(sizeof(*tac_u));
  tac_u->bug_l = bug_l;
  tac_u->mug_l = mug_l;
  tac_u->eve_d = eve_d;
  tac_u->nex_u = 0;
  tac_u->job   = job;

  c3_assert( (1ULL + log_u->sen_d) == eve_d );
  log_u->sen_d++;
  
  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    log_u->put_u.ent_u = log_u->put_u.ext_u = tac_u;
  }
  else {
    log_u->put_u.ent_u->nex_u = tac_u;
    log_u->put_u.ent_u = tac_u;
  }

  _disk_commit(log_u);
}

/* u3_disk_boot_plan():
*/
void
u3_disk_boot_plan(u3_disk* log_u, u3_noun job)
{
  u3_fact* tac_u = c3_malloc(sizeof(*tac_u));
  tac_u->mug_l = 0; //u3r_mug(job); XX
  tac_u->eve_d = ++log_u->sen_d;
  tac_u->nex_u = 0;
  tac_u->job   = job;

  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    c3_assert( 1ULL == log_u->sen_d );

    tac_u->bug_l = 0;  // XX
    log_u->put_u.ent_u = log_u->put_u.ext_u = tac_u;
  }
  else {
    tac_u->bug_l = log_u->put_u.ent_u->mug_l;  // XX
    log_u->put_u.ent_u->nex_u = tac_u;
    log_u->put_u.ent_u = tac_u;
  }

#ifdef VERBOSE_DISK
  fprintf(stderr, "disk: (%" PRIu64 "): db boot plan\r\n", tac_u->eve_d);
#endif

  _disk_commit(log_u);
}

static void
_disk_read_done_cb(uv_timer_t* tim_u)
{
  struct _cd_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;
  u3_play  pay_u = {
    .ent_u = red_u->ent_u,
    .ext_u = red_u->ext_u
  };

  c3_assert( red_u->ent_u );
  c3_assert( red_u->ext_u );

  log_u->cb_u.read_done_f(log_u->cb_u.vod_p, pay_u);
  c3_free(red_u);
}

static c3_o
_disk_read_one_cb(void* vod_p, c3_d eve_d, size_t val_i, void* val_p)
{
  struct _cd_read* red_u = vod_p;
  u3_disk* log_u = red_u->log_u;
  u3_fact* tac_u = c3_calloc(sizeof(*tac_u));
  tac_u->eve_d = eve_d;

  {
    //  xx soft?
    //
    u3_noun dat = u3ke_cue(u3i_bytes(val_i, val_p));
    u3_noun mug, job;

    if (  (c3n == u3r_cell(dat, &mug, &job))
       || (c3n == u3r_safe_word(mug, &tac_u->bug_l)) ) // XX
    {
      c3_free(tac_u);
      //  XX dispose get_u;
      log_u->cb_u.read_bail_f(log_u->cb_u.vod_p, eve_d);
      return c3n;
    }

    tac_u->job = u3k(job);
    u3z(dat);
  }

  if ( !red_u->ent_u ) {
    c3_assert( !red_u->ext_u );

    c3_assert( red_u->eve_d == eve_d );
    // tac_u->mug_l = 0;  // XX
    red_u->ent_u = red_u->ext_u = tac_u;
  }
  else {
    c3_assert( (1ULL + red_u->ent_u->eve_d) == eve_d );
    // log_u->get_u.ent_u->mug_l = tac_u->bug_l; // XX
    red_u->ent_u->nex_u = tac_u;
    red_u->ent_u = tac_u;
  }

  return c3y;
}

static void
_disk_read_start_cb(uv_timer_t* tim_u)
{
  struct _cd_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;

  uv_timer_start(&log_u->tim_u, _disk_read_done_cb, 0, 0);

  if ( c3n == c3_lmdb_read(log_u->mdb_u,
                           red_u,
                           red_u->eve_d,
                           red_u->len_d,
                           _disk_read_one_cb) )
  {
    log_u->cb_u.read_bail_f(log_u->cb_u.vod_p, red_u->eve_d);
  }
}

void
u3_disk_read(u3_disk* log_u, c3_d eve_d, c3_d len_d)
{
  struct _cd_read* red_u = c3_malloc(sizeof(*red_u));
  red_u->log_u = log_u;
  red_u->eve_d = eve_d;
  red_u->len_d = len_d;
  red_u->ent_u = red_u->ext_u = 0;

  log_u->tim_u.data = red_u;  
  uv_timer_start(&log_u->tim_u, _disk_read_start_cb, 0, 0);
}

/* u3_disk_exit(): close the log.
*/
void
u3_disk_exit(u3_disk* log_u)
{
  c3_lmdb_exit(log_u->mdb_u);
  //  XX dispose
  //
}
