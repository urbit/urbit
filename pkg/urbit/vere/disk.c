//! @file disk.c

#include "vere/disk.h"

#include "all.h"
#include "vere/vere.h"
#include "vere/db/lmdb.h"

#undef VERBOSE_DISK
#undef DISK_TRACE_JAM
#undef DISK_TRACE_CUE

struct _u3_disk_walk {
  u3_lmdb_walk  itr_u;
  u3_disk*      log_u;
  c3_o          liv_o;
};

//! Commit complete.
static void
_disk_commit_done(u3_disk* log_u)
{
  c3_d eve_d = log_u->sav_u.eve_d;
  c3_w len_w = log_u->sav_u.len_w;
  c3_o ret_o = log_u->sav_u.ret_o;

#ifdef VERBOSE_DISK
  c3_c* msg_c = ( c3Y == ret_o ) ? "complete" : "failed";

  if ( 1 == len_w ) {
    fprintf(stderr, "disk: (%" PRIu64 "): commit: %s\r\n", eve_d, msg_c);
  }
  else {
    fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: %s\r\n",
                    eve_d,
                    eve_d + (len_w - 1),
                    msg_c);
  }
#endif


  if ( c3y == ret_o ) {
    log_u->dun_d += len_w;
  }

  if ( log_u->sav_u.don_f ) {
    log_u->sav_u.don_f(log_u->sav_u.ptr_v, eve_d + (len_w - 1), ret_o);
  }

  while ( 1 ) {
    u3_feat* fet_u = c3_queue_peek_front(log_u->put_u);
    if ( !fet_u || fet_u->eve_d > log_u->dun_d ) {
      break;
    }
    c3_assert((void*)fet_u == c3_queue_pop_front(log_u->put_u));
    c3_free(fet_u->hun_y);
    c3_free(fet_u);
  }
}

static void
_disk_commit(u3_disk* log_u);

//! On the main thread, finish write
static void
_disk_commit_after_cb(uv_work_t* ted_u, c3_i sas_i)
{
  u3_disk* log_u = ted_u->data;

  log_u->sav_u.ted_o = c3n;

  if ( UV_ECANCELED != sas_i ) {
    _disk_commit_done(log_u);
    _disk_commit(log_u);
  }
}

//! Off the main thread, write event-batch.
static void
_disk_commit_cb(uv_work_t* ted_u)
{
  u3_disk* log_u = ted_u->data;

  log_u->sav_u.ret_o = u3_lmdb_save(log_u->mdb_u,
                                    log_u->sav_u.eve_d,
                                    log_u->sav_u.len_w,
                            (void**)log_u->sav_u.byt_y,
                                    log_u->sav_u.siz_i);
}

//! Queue async event-batch write.
static void
_disk_commit_start(u3_disk* log_u)
{
  c3_assert( c3n == log_u->sav_u.ted_o );
  log_u->sav_u.ted_o = c3y;

  //  queue asynchronous work to happen on another thread
  //
  uv_queue_work(u3L, &log_u->sav_u.ted_u, _disk_commit_cb,
                                          _disk_commit_after_cb);
}

c3_w
u3_disk_etch(u3_disk* log_u,
             u3_noun    eve,
             c3_l     mug_l,
             c3_y**   out_y)
{
  //  XX check version number
  //

  #ifdef DISK_TRACE_JAM
  u3t_event_trace("king disk jam", 'B');
#endif

  //  XX needs api redesign to limit allocations
  //
  {
    u3_atom mat = u3qe_jam(eve);
    c3_w  len_w = u3r_met(3, mat);
    c3_y* dat_y = c3_malloc(4 + len_w);
    dat_y[0] = mug_l & 0xff;
    dat_y[1] = (mug_l >> 8) & 0xff;
    dat_y[2] = (mug_l >> 16) & 0xff;
    dat_y[3] = (mug_l >> 24) & 0xff;
    u3r_bytes(0, len_w, dat_y + 4, mat);

#ifdef DISK_TRACE_JAM
    u3t_event_trace("king disk jam", 'E');
#endif

    u3z(mat);

    *out_y = dat_y;
    return len_w + 4;
  }
}

//! Create a write batch
static c3_o
_disk_batch(u3_disk* log_u)
{
  c3_w     len_w = log_u->sen_d - log_u->dun_d;

  if (  !len_w
     || (c3y == log_u->sav_u.ted_o) )
  {
    return c3n;
  }
  else {
    len_w = c3_min(len_w, 100);

    u3_feat*  fet_u = c3_queue_peek_front(log_u->put_u);
    c3_assert( fet_u );
    c3_assert( (1ULL + log_u->dun_d) == fet_u->eve_d );

    log_u->sav_u.ret_o = c3n;
    log_u->sav_u.eve_d = fet_u->eve_d;
    log_u->sav_u.len_w = len_w;

    for ( c3_w i_w = 0; i_w < len_w; ++i_w ) {
      c3_assert( fet_u );
      c3_assert( (log_u->sav_u.eve_d + i_w) == fet_u->eve_d );

      log_u->sav_u.byt_y[i_w] = fet_u->hun_y;
      log_u->sav_u.siz_i[i_w] = fet_u->len_i;

      fet_u = c3_queue_peek(log_u->put_u, 1 + i_w);
    }

    log_u->hit_w[len_w]++;

    return c3y;
  }
}

//! Commit all available events, if idle.
static void
_disk_commit(u3_disk* log_u)
{
  if ( c3y == _disk_batch(log_u) ) {
#ifdef VERBOSE_DISK
    if ( 1 == len_w ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: request\r\n",
                      log_u->sav_u.eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: request\r\n",
                      log_u->sav_u.eve_d,
                      (log_u->sav_u.eve_d + log_u->sav_u.len_w - 1));
    }
#endif

    _disk_commit_start(log_u);
  }
}

//! Enqueue serialized fact (feat) for persistence.
static void
_disk_plan(u3_disk* log_u,
           c3_l     mug_l,
           u3_noun    job)
{
  u3_feat* fet_u = c3_malloc(sizeof(*fet_u));
  fet_u->eve_d = ++log_u->sen_d;
  fet_u->len_i = (size_t)u3_disk_etch(log_u, job, mug_l, &fet_u->hun_y);

  c3_queue_push_back(log_u->put_u, fet_u, sizeof(*fet_u));
}

void
u3_disk_plan(u3_disk* log_u, u3_fact* tac_u)
{
  c3_assert( (1ULL + log_u->sen_d) == tac_u->eve_d );

  _disk_plan(log_u, tac_u->mug_l, tac_u->job);

  _disk_commit(log_u);
}

void
u3_disk_plan_list(u3_disk* log_u, u3_noun lit)
{
  u3_noun i, t = lit;

  while ( u3_nul != t ) {
    u3x_cell(t, &i, &t);
    //  NB, boot mugs are 0
    //
    _disk_plan(log_u, 0, i);
  }

  u3z(lit);
}

c3_o
u3_disk_sync(u3_disk* log_u)
{
  c3_o ret_o = c3n;

  //  XX max 100
  //
  if ( c3y == _disk_batch(log_u) ) {
    ret_o = u3_lmdb_save(log_u->mdb_u,
                         log_u->sav_u.eve_d,
                         log_u->sav_u.len_w,
                 (void**)log_u->sav_u.byt_y,
                         log_u->sav_u.siz_i);

    log_u->sav_u.ret_o = ret_o;

    //  XX don't want callbacks
    //
    _disk_commit_done(log_u);
  }

  return ret_o;
}

void
u3_disk_async(u3_disk*     log_u,
              void*        ptr_v,
              u3_disk_news don_f)
{
  //  XX add flag to control autosync
  //
  log_u->sav_u.ptr_v = ptr_v;
  log_u->sav_u.don_f = don_f;
}

c3_o
u3_disk_sift(u3_disk* log_u,
             size_t   len_i,
             c3_y*    dat_y,
             c3_l*    mug_l,
             u3_noun*   job)
{
  //  XX check version
  //

  if ( 4 >= len_i ) {
    return c3n;
  }
  else {
    *mug_l = dat_y[0]
           ^ (dat_y[1] <<  8)
           ^ (dat_y[2] << 16)
           ^ (dat_y[3] << 24);

#ifdef DISK_TRACE_CUE
    u3t_event_trace("king disk cue", 'B');
#endif

    //  XX u3m_soft?
    //
    *job = u3ke_cue(u3i_bytes(len_i - 4, dat_y + 4));

#ifdef DISK_TRACE_CUE
    u3t_event_trace("king disk cue", 'E');
#endif

    return c3y;
  }
}

struct _cd_list {
  u3_disk* log_u;
  u3_noun    eve;
  c3_l     mug_l;
};

//! Lmdb read callback, invoked for each event in order
static c3_o
_disk_read_list_cb(void* ptr_v, c3_d eve_d, size_t val_i, void* val_p)
{
  struct _cd_list* ven_u = ptr_v;
  u3_disk* log_u = ven_u->log_u;

  {
    u3_noun job;
    c3_l  mug_l;

    if ( c3n == u3_disk_sift(log_u, val_i, (c3_y*)val_p, &mug_l, &job) ) {
      return c3n;
    }

    ven_u->mug_l = mug_l;
    ven_u->eve   = u3nc(job, ven_u->eve);
  }

  return c3y;
}

u3_weak
u3_disk_read_list(u3_disk* log_u, c3_d eve_d, c3_d len_d, c3_l* mug_l)
{
  struct _cd_list ven_u = { log_u, u3_nul, 0 };

  if ( c3n == u3_lmdb_read(log_u->mdb_u,
                           &ven_u,
                           eve_d,
                           len_d,
                           _disk_read_list_cb) )
  {
    return u3_none;
  }
  else {
    *mug_l = ven_u.mug_l;
    return u3kb_flop(ven_u.eve);
  }
}

u3_disk_walk*
u3_disk_walk_init(u3_disk* log_u,
                  c3_d     eve_d,
                  c3_d     len_d)
{
  u3_disk_walk* wok_u = c3_malloc(sizeof(*wok_u));
  c3_d          max_d = eve_d + len_d - 1;

  wok_u->log_u = log_u;
  wok_u->liv_o = u3_lmdb_walk_init(log_u->mdb_u,
                                  &wok_u->itr_u,
                                   eve_d,
                                   c3_min(max_d, log_u->dun_d));

  return wok_u;
}

c3_o
u3_disk_walk_live(u3_disk_walk* wok_u)
{
  if ( wok_u->itr_u.nex_d > wok_u->itr_u.las_d ) {
    wok_u->liv_o = c3n;
  }

  return wok_u->liv_o;
}

c3_o
u3_disk_walk_step(u3_disk_walk* wok_u, u3_fact* tac_u)
{
  u3_disk* log_u = wok_u->log_u;
  size_t   len_i;
  void*    buf_v;

  tac_u->eve_d = wok_u->itr_u.nex_d;

  if ( c3n == u3_lmdb_walk_next(&wok_u->itr_u, &len_i, &buf_v) ) {
    fprintf(stderr, "disk: (%" PRIu64 "): read fail\r\n", tac_u->eve_d);
    return wok_u->liv_o = c3n;
  }

  if ( c3n == u3_disk_sift(log_u, len_i,
                           (c3_y*)buf_v,
                           &tac_u->mug_l,
                           &tac_u->job) )
  {
    fprintf(stderr, "disk: (%" PRIu64 "): sift fail\r\n", tac_u->eve_d);
    return wok_u->liv_o = c3n;
  }

  return c3y;
}

void
u3_disk_walk_done(u3_disk_walk* wok_u)
{
  u3_lmdb_walk_done(&wok_u->itr_u);
  c3_free(wok_u);
}

//! Serialize atom, save as metadata at [key_c].
static c3_o
_disk_save_meta(u3_disk* log_u, const c3_c* key_c, u3_atom dat)
{
  c3_w  len_w = u3r_met(3, dat);
  c3_y* byt_y = c3_malloc(len_w);
  u3r_bytes(0, len_w, byt_y, dat);

  {
    c3_o ret_o = u3_lmdb_save_meta(log_u->mdb_u, key_c, len_w, byt_y);
    c3_free(byt_y);
    return ret_o;
  }
}

c3_o
u3_disk_save_meta(u3_disk* log_u, const u3_meta* met_u)
{
  c3_assert( c3y == u3a_is_cat(met_u->lif_w) );

  u3_noun who = u3i_chubs(2, met_u->who_d);

  if (  (c3n == _disk_save_meta(log_u, "version", 1))
     || (c3n == _disk_save_meta(log_u, "who", who))
     || (c3n == _disk_save_meta(log_u, "fake", met_u->fak_o))
     || (c3n == _disk_save_meta(log_u, "life", met_u->lif_w)) )
  {
    u3z(who);
    return c3n;
  }

  u3z(who);
  return c3y;
}

//! Copy array to an atom.
//!
//! @param[out] dst_v  Address of destination atom.
//! @param[in]  len_i  Length of source array in bytes.
//! @param[in]  val_v  Source array.
//!
//! @n (1) Confirm unique, non-NULL source and destination addresses.
static void
_disk_meta_read_cb(void* dst_v, size_t len_i, void* src_v)
{
  // (1)
  if ( src_v == dst_v ) {
    return;
  }

  *(u3_atom*)dst_v = u3i_bytes(len_i, src_v);
}

//! Read metadata at [key_c], deserialize.
static u3_weak
_disk_read_meta(u3_disk* log_u, const c3_c* key_c)
{
  u3_weak dat = u3_none;
  u3_lmdb_read_meta(log_u->mdb_u, &dat, key_c, _disk_meta_read_cb);
  return dat;
}

c3_o
u3_disk_read_meta(u3_disk* log_u, u3_meta* met_u)
{
  u3_weak ver, who, fak, lif;

  if ( u3_none == (ver = _disk_read_meta(log_u, "version")) ) {
    fprintf(stderr, "disk: read meta: no version\r\n");
    return c3n;
  }
  if ( u3_none == (who = _disk_read_meta(log_u, "who")) ) {
    fprintf(stderr, "disk: read meta: no indentity\r\n");
    return c3n;
  }
  if ( u3_none == (fak = _disk_read_meta(log_u, "fake")) ) {
    fprintf(stderr, "disk: read meta: no fake bit\r\n");
    return c3n;
  }
  if ( u3_none == (lif = _disk_read_meta(log_u, "life")) ) {
    fprintf(stderr, "disk: read meta: no lifecycle length\r\n");
    return c3n;
  }

  {
    c3_o val_o = c3y;

    if ( 1 != ver ) {
      fprintf(stderr, "disk: read meta: unknown version %u\r\n", ver);
      val_o = c3n;
    }
    else if ( !((c3y == fak ) || (c3n == fak )) ) {
      fprintf(stderr, "disk: read meta: invalid fake bit\r\n");
      val_o = c3n;
    }
    else if ( c3n == u3a_is_cat(lif) ) {
      fprintf(stderr, "disk: read meta: invalid lifecycle length\r\n");
      val_o = c3n;
    }

    if ( c3n == val_o ) {
      u3z(ver); u3z(who); u3z(fak); u3z(lif);
      return c3n;
    }
  }

  u3r_chubs(0, 2, met_u->who_d, who);
  met_u->fak_o = fak;
  met_u->lif_w = lif;

  u3z(who);
  return c3y;
}

//! @n (1) Cancel write thread.
//!        XX can deadlock when called from signal handler.
//!        XX revise SIGTSTP handling.
//! @n (2) Close database.
//! @n (3) Dispose of planned writes.
void
u3_disk_exit(u3_disk* log_u)
{
  // (1)
  if ( c3y == log_u->sav_u.ted_o ) {
    c3_i sas_i;

    do {
      sas_i = uv_cancel(&log_u->sav_u.req_u);
    }
    while ( UV_EBUSY == sas_i );
  }

  // (2)
  u3_lmdb_exit(log_u->mdb_u);

  // (3)
  {
    while ( 1 ) {
      u3_feat* fet_u = c3_queue_peek_front(log_u->put_u);
      if ( !fet_u || fet_u->eve_d > log_u->dun_d ) {
        break;
      }
      c3_assert((void*)fet_u == c3_queue_pop_front(log_u->put_u));
      c3_free(fet_u->hun_y);
      c3_free(fet_u);
    }
  }

  c3_free(log_u->dir_c);
  c3_free(log_u->urb_c);
  c3_free(log_u->log_c);
  c3_queue_free(log_u->put_u);

  c3_free(log_u);

#if defined(DISK_TRACE_JAM) || defined(DISK_TRACE_CUE)
  u3t_trace_close();
#endif
}

void
u3_disk_info(u3_disk* log_u)
{
  u3l_log("  disk: live=%s, event=%" PRIu64 "\n",
          ( c3y == log_u->liv_o ) ? "&" : "|",
          log_u->dun_d);

  {
    c3_w len_w, i_w;

    u3l_log("    batch:\n");

    for ( i_w = 0; i_w < c3_arr_len(log_u->hit_w); i_w++ ) {
      len_w = log_u->hit_w[i_w];
      if ( len_w ) {
        u3l_log("      %u: %u\n", i_w, len_w);
      }
    }
  }

  switch ( c3_queue_length(log_u->put_u) ) {
    case 0:
      break;
    case 1:
      u3l_log("    save: %" PRIu64 "\n",
              ((u3_feat*)c3_queue_peek_front(log_u->put_u))->eve_d);
      break;
    default:
      u3l_log("    save: %" PRIu64 "-%" PRIu64 "\n",
              ((u3_feat*)c3_queue_peek_front(log_u->put_u))->eve_d,
              ((u3_feat*)c3_queue_peek_back(log_u->put_u))->eve_d);
      break;
  }
}

//! @n (1) Create/load the pier directory.
//! @n (2) Create/load `$pier/.urb`.
//! @n (3) Create/load `$pier/.urb/put`.
//! @n (4) Create/load `$pier/.urb/get`.
//! @n (5) Create/load `$pier/.urb/log`.
//! @n (6) Initialize database. Arbitrarily choose 1TB as a "large enough" mapsize.
//!        Per the LMDB docs:
//!        "[..] on 64-bit there is no penalty for making this huge (say 1TB)."
//! @n (7) Get the latest event number from the db.
u3_disk*
u3_disk_init(c3_c* pax_c)
{
  u3_disk* log_u = c3_calloc(sizeof(*log_u));
  log_u->liv_o = c3n;
  log_u->sav_u.ted_o = c3n;
  log_u->sav_u.ted_u.data = log_u;

  // (1)
  {
    log_u->dir_c = c3_malloc(1 + strlen(pax_c));
    c3_assert(NULL != log_u->dir_c);
    c3_assert(log_u->dir_c == strcpy(log_u->dir_c, pax_c));
    if ( -1 == mkdir(log_u->dir_c, 0700) && EEXIST != errno ) {
      fprintf(stderr, "disk: failed to load pier at %s\r\n", log_u->dir_c);
      c3_free(log_u->dir_c);
      c3_free(log_u);
      return 0;
    }
  }

  // (2)
  {
    static const c3_c urb_c[] = "/.urb";
    c3_ws siz_ws = strlen(log_u->dir_c) + sizeof(urb_c);
    log_u->urb_c = c3_malloc(siz_ws);
    c3_assert(NULL != log_u->urb_c);
    c3_assert(siz_ws - 1 == snprintf(log_u->urb_c, siz_ws, "%s%s", log_u->dir_c, urb_c));
    if ( -1 == mkdir(log_u->urb_c, 0700) && EEXIST != errno ) {
      fprintf(stderr, "disk: failed to load %s in %s\r\n", urb_c, log_u->dir_c);
      c3_free(log_u->dir_c);
      c3_free(log_u->urb_c);
      c3_free(log_u);
      return 0;
    }
  }


  {
    static const c3_c put_c[] = "/put";
    static const c3_c get_c[] = "/get";
    _Static_assert(sizeof(put_c) == sizeof(get_c));
    c3_ws siz_ws = strlen(log_u->urb_c) + c3_max(sizeof(put_c), sizeof(get_c));
    c3_c dir_c[siz_ws];

    // (3)
    c3_assert(siz_ws - 1 == snprintf(dir_c, siz_ws, "%s%s", log_u->urb_c, put_c));
    if ( -1 == mkdir(dir_c, 0700) && EEXIST != errno ) {
      fprintf(stderr, "disk: failed to load %s in %s\r\n", put_c, log_u->urb_c);
      c3_free(log_u->dir_c);
      c3_free(log_u->urb_c);
      c3_free(log_u);
      return 0;
    }

    // (4)
    c3_assert(siz_ws - 1 == snprintf(dir_c, siz_ws, "%s%s", log_u->urb_c, get_c));
    if ( -1 == mkdir(dir_c, 0700) && EEXIST != errno ) {
      fprintf(stderr, "disk: failed to load %s in %s\r\n", get_c, log_u->urb_c);
      c3_free(log_u->dir_c);
      c3_free(log_u->urb_c);
      c3_free(log_u);
      return 0;
    }
  }

  // (5)
  {
    static const c3_c log_c[] = "/log";
    c3_ws siz_ws = strlen(log_u->urb_c) + sizeof(log_c);
    log_u->log_c = c3_malloc(siz_ws);
    c3_assert(NULL != log_u->log_c);
    c3_assert(siz_ws - 1 == snprintf(log_u->log_c, siz_ws, "%s%s", log_u->urb_c, log_c));
    if ( -1 == mkdir(log_u->log_c, 0700) && EEXIST != errno ) {
      fprintf(stderr, "disk: failed to load %s in %s\r\n", log_u->log_c, log_u->urb_c);
      c3_free(log_u->dir_c);
      c3_free(log_u->urb_c);
      c3_free(log_u->log_c);
      c3_free(log_u);
      return 0;
    }
  }

  // (6)
  {
    static const size_t siz_i =
#if (defined(U3_CPU_aarch64) && defined(U3_OS_linux)) || defined(U3_OS_mingw)
    0xf00000000;
#else
    0x10000000000;
#endif
    if ( 0 == (log_u->mdb_u = u3_lmdb_init(log_u->log_c, siz_i)) ) {
      fprintf(stderr, "disk: failed to initialize database\r\n");
      c3_free(log_u);
      return 0;
    }
  }

  // (7)
  {
    log_u->dun_d = 0;
    c3_d fir_d;
    if ( c3n == u3_lmdb_gulf(log_u->mdb_u, &fir_d, &log_u->dun_d) ) {
      fprintf(stderr, "disk: failed to load latest event from database\r\n");
      c3_free(log_u);
      return 0;
    }
    log_u->sen_d = log_u->dun_d;
  }

#if defined(DISK_TRACE_JAM) || defined(DISK_TRACE_CUE)
  u3t_trace_open(pax_c);
#endif

  log_u->liv_o = c3y;
  log_u->put_u = c3_queue_init();

  return log_u;
}
