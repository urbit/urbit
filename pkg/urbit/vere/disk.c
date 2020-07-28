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
  uv_timer_t       tim_u;
  c3_d             eve_d;
  c3_d             len_d;
  struct _u3_fact* ent_u;               //  queue entry
  struct _u3_fact* ext_u;               //  queue exit
  struct _u3_disk* log_u;
};

struct _cd_save {
  c3_o             ret_o;               //  result
  c3_d             eve_d;               //  first event
  c3_d             len_d;               //  number of events
  c3_y**           byt_y;               //  array of bytes
  size_t*          siz_i;               //  array of lengths
  struct _u3_disk* log_u;
};

#undef VERBOSE_DISK

static void
_disk_commit(u3_disk* log_u);

/* _disk_free_save(): free write batch
*/
static void
_disk_free_save(struct _cd_save* req_u)
{
  while ( req_u->len_d-- ) {
    c3_free(req_u->byt_y[req_u->len_d]);
  }

  c3_free(req_u->byt_y);
  c3_free(req_u->siz_i);
  c3_free(req_u);
}

/* _disk_commit_done(): commit complete.
 */
static void
_disk_commit_done(struct _cd_save* req_u)
{
  u3_disk* log_u = req_u->log_u;
  c3_d     eve_d = req_u->eve_d;
  c3_d     len_d = req_u->len_d;
  c3_o     ret_o = req_u->ret_o;

  if ( c3n == ret_o ) {
    log_u->cb_u.write_bail_f(log_u->cb_u.ptr_v, eve_d + (len_d - 1ULL));

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
    log_u->cb_u.write_done_f(log_u->cb_u.ptr_v, log_u->dun_d);

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
      u3_fact_free(tac_u);
      tac_u = log_u->put_u.ext_u;
    }
  }

  if ( !log_u->put_u.ext_u ) {
    log_u->put_u.ent_u = 0;
  }

  _disk_free_save(req_u);

  _disk_commit(log_u);
}

/* _disk_commit_after_cb(): on the main thread, finish write
*/
static void
_disk_commit_after_cb(uv_work_t* ted_u, c3_i sas_i)
{
  struct _cd_save* req_u = ted_u->data;

  if ( UV_ECANCELED == sas_i ) {
    _disk_free_save(req_u);
  }
  else {
    ted_u->data = 0;
    req_u->log_u->ted_o = c3n;
    _disk_commit_done(req_u);
  }
}

/* _disk_commit_cb(): off the main thread, write event-batch.
*/
static void
_disk_commit_cb(uv_work_t* ted_u)
{
  struct _cd_save* req_u = ted_u->data;
  req_u->ret_o = u3_lmdb_save(req_u->log_u->mdb_u,
                              req_u->eve_d,
                              req_u->len_d,
                              (void**)req_u->byt_y, // XX safe?
                              req_u->siz_i);
}

/* _disk_commit_start(): queue async event-batch write.
*/
static void
_disk_commit_start(struct _cd_save* req_u)
{
  u3_disk* log_u = req_u->log_u;

  c3_assert( c3n == log_u->ted_o );
  log_u->ted_o = c3y;
  log_u->ted_u.data = req_u;

  //  queue asynchronous work to happen on another thread
  //
  uv_queue_work(u3L, &log_u->ted_u, _disk_commit_cb,
                                    _disk_commit_after_cb);
}

/* _disk_serialize_v0(): serialize events in format v0.
*/
static c3_w
_disk_serialize_v0(u3_fact* tac_u, c3_y** dat_y)
{
  u3_atom mat = u3ke_jam(u3nc(tac_u->bug_l, u3k(tac_u->job)));
  c3_w  len_w = u3r_met(3, mat);
  *dat_y = c3_malloc(len_w);
  u3r_bytes(0, len_w, *dat_y, mat);

  u3z(mat);

  return len_w;
}

/* _disk_batch(): create a write batch
*/
static struct _cd_save*
_disk_batch(u3_disk* log_u, c3_d len_d)
{
  u3_fact* tac_u = log_u->put_u.ext_u;

  c3_assert( (1ULL + log_u->dun_d) == tac_u->eve_d );
  c3_assert( log_u->sen_d == log_u->put_u.ent_u->eve_d );

  struct _cd_save* req_u = c3_malloc(sizeof(*req_u));
  req_u->log_u = log_u;
  req_u->ret_o = c3n;
  req_u->eve_d = tac_u->eve_d;
  req_u->len_d = len_d;
  req_u->byt_y = c3_malloc(len_d * sizeof(c3_y*));
  req_u->siz_i = c3_malloc(len_d * sizeof(size_t));

  for ( c3_d i_d = 0ULL; i_d < len_d; ++i_d) {
    c3_assert( (req_u->eve_d + i_d) == tac_u->eve_d );

    req_u->siz_i[i_d] = _disk_serialize_v0(tac_u, &req_u->byt_y[i_d]);

    tac_u = tac_u->nex_u;
  }

  return req_u;
}

/* _disk_commit(): commit all available events, if idle.
*/
static void
_disk_commit(u3_disk* log_u)
{
  if (  (c3n == log_u->ted_o)
     && (log_u->sen_d > log_u->dun_d) )
  {
    c3_d len_d = log_u->sen_d - log_u->dun_d;
    struct _cd_save* req_u = _disk_batch(log_u, len_d);

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: request\r\n",
                      req_u->eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: request\r\n",
                      req_u->eve_d,
                      (req_u->eve_d + len_d - 1ULL));
    }
#endif

    _disk_commit_start(req_u);
  }
}

/* u3_disk_plan(): enqueue completed event for persistence.
*/
void
u3_disk_plan(u3_disk* log_u, u3_fact* tac_u)
{
  c3_assert( (1ULL + log_u->sen_d) == tac_u->eve_d );
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

/* u3_disk_boot_plan(): enqueue boot sequence, without autocommit.
*/
void
u3_disk_boot_plan(u3_disk* log_u, u3_noun job)
{
  //  NB, boot mugs are 0
  //
  u3_fact* tac_u = u3_fact_init(++log_u->sen_d, 0, job);
  tac_u->bug_l   = 0;  // XX

  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    c3_assert( 1ULL == log_u->sen_d );

    log_u->put_u.ent_u = log_u->put_u.ext_u = tac_u;
  }
  else {
    log_u->put_u.ent_u->nex_u = tac_u;
    log_u->put_u.ent_u = tac_u;
  }

#ifdef VERBOSE_DISK
  fprintf(stderr, "disk: (%" PRIu64 "): db boot plan\r\n", tac_u->eve_d);
#endif
}

/* u3_disk_boot_save(): commit boot sequence.
*/
void
u3_disk_boot_save(u3_disk* log_u)
{
  c3_assert( !log_u->dun_d );
  _disk_commit(log_u);
}

static void
_disk_read_free(u3_read* red_u)
{
  //  free facts (if the read failed)
  //
  {
    u3_fact* tac_u = red_u->ext_u;
    u3_fact* nex_u;

    while ( tac_u ) {
      nex_u = tac_u->nex_u;
      u3_fact_free(tac_u);
      tac_u = nex_u;
    }
  }

  c3_free(red_u);
}

/* _disk_read_close_cb():
*/
static void
_disk_read_close_cb(uv_handle_t* had_u)
{
  u3_read* red_u = had_u->data;
  _disk_read_free(red_u);
}

static void
_disk_read_close(u3_read* red_u)
{
  u3_disk* log_u = red_u->log_u;

  //  unlink request
  //
  {
    if ( red_u->pre_u ) {
      red_u->pre_u->nex_u = red_u->nex_u;
    }
    else {
      log_u->red_u = red_u->nex_u;
    }

    if ( red_u->nex_u ) {
      red_u->nex_u->pre_u = red_u->pre_u;
    }
  }

  uv_close(&red_u->had_u, _disk_read_close_cb);
}

/* _disk_read_done_cb(): finalize read, invoke callback with response.
*/
static void
_disk_read_done_cb(uv_timer_t* tim_u)
{
  u3_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;
  u3_info  pay_u = { .ent_u = red_u->ent_u, .ext_u = red_u->ext_u };

  c3_assert( red_u->ent_u );
  c3_assert( red_u->ext_u );
  red_u->ent_u = 0;
  red_u->ext_u = 0;

  log_u->cb_u.read_done_f(log_u->cb_u.ptr_v, pay_u);
  _disk_read_close(red_u);
}

/* _disk_read_one_cb(): lmdb read callback, invoked for each event in order
*/
static c3_o
_disk_read_one_cb(void* ptr_v, c3_d eve_d, size_t val_i, void* val_p)
{
  u3_read* red_u = ptr_v;
  u3_disk* log_u = red_u->log_u;
  u3_fact* tac_u;

  {
    //  XX u3m_soft?
    //
    u3_noun dat = u3ke_cue(u3i_bytes(val_i, val_p));
    u3_noun mug, job;
    c3_l  bug_l;


    if (  (c3n == u3r_cell(dat, &mug, &job))
       || (c3n == u3r_safe_word(mug, &bug_l)) ) // XX
    {
      //  failure here triggers cleanup in _disk_read_start_cb()
      //
      u3z(dat);
      return c3n;
    }

    //  NB: mug is unknown due to log format
    //
    tac_u = u3_fact_init(eve_d, 0, u3k(job));
    tac_u->bug_l = bug_l;

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

/* _disk_read_start_cb(): the read from the db, trigger response
*/
static void
_disk_read_start_cb(uv_timer_t* tim_u)
{
  u3_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;

  //  read events synchronously
  //
  if ( c3n == u3_lmdb_read(log_u->mdb_u,
                           red_u,
                           red_u->eve_d,
                           red_u->len_d,
                           _disk_read_one_cb) )
  {
    log_u->cb_u.read_bail_f(log_u->cb_u.ptr_v, red_u->eve_d);
    _disk_read_close(red_u);
  }
  //  finish the read asynchronously
  //
  else {
    uv_timer_start(&red_u->tim_u, _disk_read_done_cb, 0, 0);
  }
}

/* u3_disk_read(): read [len_d] events starting at [eve_d].
*/
void
u3_disk_read(u3_disk* log_u, c3_d eve_d, c3_d len_d)
{
  u3_read* red_u = c3_malloc(sizeof(*red_u));
  red_u->log_u = log_u;
  red_u->eve_d = eve_d;
  red_u->len_d = len_d;
  red_u->ent_u = red_u->ext_u = 0;
  red_u->pre_u = 0;
  red_u->nex_u = log_u->red_u;

  if ( log_u->red_u ) {
    log_u->red_u->pre_u = red_u;
  }
  log_u->red_u = red_u;

  //  perform the read asynchronously
  //
  uv_timer_init(u3L, &red_u->tim_u);

  red_u->tim_u.data = red_u;
  uv_timer_start(&red_u->tim_u, _disk_read_start_cb, 0, 0);
}

/* _disk_save_meta(): serialize atom, save as metadata at [key_c].
*/
static c3_o
_disk_save_meta(u3_disk* log_u, const c3_c* key_c, u3_atom dat)
{
  u3_atom mat = u3ke_jam(dat);
  c3_w  len_w = u3r_met(3, mat);
  c3_y* byt_y = c3_malloc(len_w);
  c3_o  ret_o;

  u3r_bytes(0, len_w, byt_y, mat);

  ret_o = u3_lmdb_save_meta(log_u->mdb_u, key_c, len_w, byt_y);

  u3z(mat);
  c3_free(byt_y);

  return ret_o;
}

/* u3_disk_save_meta(): save metadata.
*/
c3_o
u3_disk_save_meta(u3_disk* log_u,
                  c3_d     who_d[2],
                  c3_o     fak_o,
                  c3_w     lif_w)
{
  c3_assert( c3y == u3a_is_cat(lif_w) );

  if (  (c3n == _disk_save_meta(log_u, "who", u3i_chubs(2, who_d)))
     || (c3n == _disk_save_meta(log_u, "is-fake", fak_o))
     || (c3n == _disk_save_meta(log_u, "life", lif_w)) )
  {
    return c3n;
  }

  return c3y;
}

/* _disk_meta_read_cb(): copy [val_p] to atom [ptr_v] if present.
*/
static void
_disk_meta_read_cb(void* ptr_v, size_t val_i, void* val_p)
{
  u3_weak* mat = ptr_v;

  if ( val_p ) {
    *mat = u3i_bytes(val_i, val_p);
  }
}

/* _disk_read_meta(): read metadata at [key_c], deserialize.
*/
static u3_weak
_disk_read_meta(u3_disk* log_u, const c3_c* key_c)
{
  u3_weak mat = u3_none;
  u3_weak dat = u3_none;
  u3_noun pro;

  u3_lmdb_read_meta(log_u->mdb_u, &mat, key_c, _disk_meta_read_cb);

  if ( u3_none != mat ) {
    pro = u3m_soft(0, u3ke_cue, mat);

    if ( u3_blip != u3h(pro) ) {
      fprintf(stderr, "disk: meta cue failed\r\n");
    }
    else {
      dat = u3k(u3t(pro));
    }
  }

  u3z(pro);
  return dat;
}

/* u3_disk_read_meta(): read metadata.
*/
c3_o
u3_disk_read_meta(u3_disk* log_u,
                  c3_d*    who_d,
                  c3_o*    fak_o,
                  c3_w*    lif_w)
{
  u3_weak who = _disk_read_meta(log_u, "who");
  u3_weak fak = _disk_read_meta(log_u, "is-fake");
  u3_weak lif = _disk_read_meta(log_u, "life");

  if ( u3_none == who ) {
    fprintf(stderr, "disk: read meta: no indentity\r\n");
    return c3n;
  }
  else if ( u3_none == fak ) {
    fprintf(stderr, "disk: read meta: no fake bit\r\n");
    u3z(who);
    return c3n;
  }
  else if ( u3_none == lif ) {
    fprintf(stderr, "disk: read meta: no lifecycle length\r\n");
    u3z(who);
    return c3n;
  }

  if ( !((c3y == fak ) || (c3n == fak )) ) {
    fprintf(stderr, "disk: read meta: invalid fake bit\r\n");
    u3z(who); u3z(fak); u3z(lif);
    return c3n;
  }
  else if ( c3n == u3a_is_cat(lif) ) {
    fprintf(stderr, "disk: read meta: invalid lifecycle length\r\n");
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

/* u3_disk_exit(): close the log.
*/
void
u3_disk_exit(u3_disk* log_u)
{
  //  cancel all outstanding reads
  //
  {
    u3_read* red_u = log_u->red_u;

    while ( red_u ) {
      _disk_read_close(red_u);
      red_u = red_u->nex_u;
    }
  }

  //  cancel write thread
  //
  if ( c3y == log_u->ted_o ) {
    c3_i sas_i;

    do {
      sas_i = uv_cancel(&log_u->req_u);
    }
    while ( UV_EBUSY == sas_i );
  }

  //  close database
  //
  u3_lmdb_exit(log_u->mdb_u);

  //  dispose planned writes
  //

  {
    u3_fact* tac_u = log_u->put_u.ext_u;
    u3_fact* nex_u;

    while ( tac_u ) {
      nex_u = tac_u->nex_u;
      u3_fact_free(tac_u);
      tac_u = nex_u;
    }
  }

  u3_dire_free(log_u->dir_u);
  u3_dire_free(log_u->urb_u);
  u3_dire_free(log_u->com_u);

  c3_free(log_u);
}

/* u3_disk_info(): print status info.
*/
void
u3_disk_info(u3_disk* log_u)
{
  u3l_log("  disk: live=%s, event=%" PRIu64 "\n",
          ( c3y == log_u->liv_o ) ? "&" : "|",
          log_u->dun_d);

  {
    u3_read* red_u = log_u->red_u;

    while ( red_u ) {
      u3l_log("    read: %" PRIu64 "-%" PRIu64 "\n",
              red_u->eve_d,
              (red_u->eve_d + red_u->len_d) - 1);
    }
  }

  if ( log_u->put_u.ext_u ) {
    if ( log_u->put_u.ext_u != log_u->put_u.ent_u ) {
      u3l_log("    save: %" PRIu64 "-%" PRIu64 "\n",
              log_u->put_u.ext_u->eve_d,
              log_u->put_u.ent_u->eve_d);
    }
    else {
      u3l_log("    save: %" PRIu64 "\n", log_u->put_u.ext_u->eve_d);
    }
  }
}

/* u3_disk_init(): load or create pier directories and event log.
*/
u3_disk*
u3_disk_init(c3_c* pax_c, u3_disk_cb cb_u)
{
  u3_disk* log_u = c3_calloc(sizeof(*log_u));
  log_u->liv_o = c3n;
  log_u->ted_o = c3n;
  log_u->cb_u  = cb_u;
  log_u->red_u = 0;
  log_u->put_u.ent_u = log_u->put_u.ext_u = 0;

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

    //  Arbitrarily choosing 1TB as a "large enough" mapsize
    //
    //  per the LMDB docs:
    //  "[..] on 64-bit there is no penalty for making this huge (say 1TB)."
    //
    {
      #if defined(U3_CPU_aarch64) && defined(U3_OS_linux)
        const size_t siz_i = 64424509440;
      #else
        const size_t siz_i = 1099511627776;
      #endif

      if ( 0 == (log_u->mdb_u = u3_lmdb_init(log_c, siz_i)) ) {
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

    if ( c3n == u3_lmdb_gulf(log_u->mdb_u, &fir_d, &log_u->dun_d) ) {
      fprintf(stderr, "disk: failed to load latest event from database");
      c3_free(log_u);
      return 0;
    }

    log_u->sen_d = log_u->dun_d;
  }

  log_u->liv_o = c3y;

  return log_u;
}
