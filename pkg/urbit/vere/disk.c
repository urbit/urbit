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

  {
    u3_feat* fet_u = log_u->put_u.ext_u;

    while ( fet_u && (fet_u->eve_d <= log_u->dun_d) ) {
      log_u->put_u.ext_u = fet_u->nex_u;
      c3_free(fet_u->hun_y);
      c3_free(fet_u);
      fet_u = log_u->put_u.ext_u;
    }
  }

  if ( !log_u->put_u.ext_u ) {
    log_u->put_u.ent_u = 0;
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
  u3_feat* fet_u = log_u->put_u.ext_u;
  c3_w     len_w = log_u->sen_d - log_u->dun_d;

  if (  !len_w
     || (c3y == log_u->sav_u.ted_o) )
  {
    return c3n;
  }
  else {
    len_w = c3_min(len_w, 100);

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

      fet_u = fet_u->nex_u;
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
  fet_u->nex_u = 0;

  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    log_u->put_u.ent_u = log_u->put_u.ext_u = fet_u;
  }
  else {
    log_u->put_u.ent_u->nex_u = fet_u;
    log_u->put_u.ent_u = fet_u;
  }
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

//! Copy [val_p] to atom [ptr_v] if present.
static void
_disk_meta_read_cb(void* ptr_v, size_t val_i, void* val_p)
{
  u3_weak* mat = ptr_v;

  if ( val_p ) {
    *mat = u3i_bytes(val_i, val_p);
  }
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

void
u3_disk_exit(u3_disk* log_u)
{
  //  cancel write thread
  //
  //    XX can deadlock when called from signal handler
  //    XX revise SIGTSTP handling
  //
  if ( c3y == log_u->sav_u.ted_o ) {
    c3_i sas_i;

    do {
      sas_i = uv_cancel(&log_u->sav_u.req_u);
    }
    while ( UV_EBUSY == sas_i );
  }

  //  close database
  //
  u3_lmdb_exit(log_u->mdb_u);

  //  dispose planned writes
  //

  {
    u3_feat* fet_u = log_u->put_u.ext_u;

    while ( fet_u && (fet_u->eve_d <= log_u->dun_d) ) {
      log_u->put_u.ext_u = fet_u->nex_u;
      c3_free(fet_u->hun_y);
      c3_free(fet_u);
      fet_u = log_u->put_u.ext_u;
    }
  }

  u3_dire_free(log_u->dir_u);
  u3_dire_free(log_u->urb_u);
  u3_dire_free(log_u->com_u);

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

    for ( i_w = 0; i_w < 100; i_w++ ) {
      len_w = log_u->hit_w[i_w];
      if ( len_w ) {
        u3l_log("      %u: %u\n", i_w, len_w);
      }
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
  log_u->put_u.ent_u = log_u->put_u.ext_u = 0;

  // (1)
  if ( 0 == (log_u->dir_u = u3_foil_folder(pax_c)) ) {
    fprintf(stderr, "disk: failed to load pier at %s\r\n", pax_c);
    c3_free(log_u);
    return 0;
  }

  static const c3_c urb_c[] = "/.urb";
  static const c3_c put_c[] = "/put";
  static const c3_c get_c[] = "/get";
  static const c3_c log_c[] = "/log";
  static const c3_w sub_w = c3_max(c3_max(sizeof(put_c), sizeof(get_c)), sizeof(log_c));
  c3_c dir_c[strlen(pax_c) + sizeof(urb_c) + sub_w];

  // (2)
  snprintf(dir_c, sizeof(dir_c), "%s%s", pax_c, urb_c);
  fprintf(stderr, "peter: %s\r\n", dir_c);
  if ( 0 == (log_u->urb_u = u3_foil_folder(dir_c)) ) {
    fprintf(stderr, "disk: failed to load %s in %s\r\n", urb_c, pax_c);
    c3_free(log_u);
    return 0;
  }

  // (3)
  snprintf(dir_c, sizeof(dir_c), "%s%s%s", pax_c, urb_c, put_c);
  fprintf(stderr, "peter: %s\r\n", dir_c);
  mkdir(dir_c, 0700);

  // (4)
  snprintf(dir_c, sizeof(dir_c), "%s%s%s", pax_c, urb_c, get_c);
  fprintf(stderr, "peter: %s\r\n", dir_c);
  mkdir(dir_c, 0700);

  // (5)
  snprintf(dir_c, sizeof(dir_c), "%s%s%s", pax_c, urb_c, log_c);
  fprintf(stderr, "peter: %s\r\n", dir_c);
  if ( 0 == (log_u->com_u = u3_foil_folder(dir_c)) ) {
    fprintf(stderr, "disk: failed to load %s%s in %s\r\n", urb_c, log_c, pax_c);
    c3_free(log_u);
    return 0;
  }

  // (6)
  static const size_t siz_i =
  #if (defined(U3_CPU_aarch64) && defined(U3_OS_linux)) || defined(U3_OS_mingw)
    0xf00000000;
  #else
    0x10000000000;
  #endif
  if ( 0 == (log_u->mdb_u = u3_lmdb_init(dir_c, siz_i)) ) {
    fprintf(stderr, "disk: failed to initialize database\r\n");
    c3_free(log_u);
    return 0;
  }

  // (7)
  log_u->dun_d = 0;
  c3_d fir_d;
  if ( c3n == u3_lmdb_gulf(log_u->mdb_u, &fir_d, &log_u->dun_d) ) {
    fprintf(stderr, "disk: failed to load latest event from database\r\n");
    c3_free(log_u);
    return 0;
  }
  log_u->sen_d = log_u->dun_d;

#if defined(DISK_TRACE_JAM) || defined(DISK_TRACE_CUE)
  u3t_trace_open(pax_c);
#endif

  log_u->liv_o = c3y;
  return log_u;
}
