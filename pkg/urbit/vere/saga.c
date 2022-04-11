//! @file saga.c
//!
//! Epoch-backed event log.

#include "vere/saga.h"

#include "all.h"
#include "c/bile.h"

//==============================================================================
// Types
//==============================================================================

//! Event log. Typedefed to `u3_saga`.
struct _u3_saga {
  c3_path*     pax_u;   //!< path to event log directory
  c3_d         eve_d;   //!< ID of youngest event
  struct {
    c3_list*   lis_u;   //!< list of epochs (front is oldest, back is youngest)
    u3_epoc*   cur_u;   //!< current epoch
  } epo_u;              //!< epochs
  struct {
    c3_list*   lis_u;   //!< list of events pending commit
    size_t     req_i;   //!< number of events in commit request
  } eve_u;              //!< events pending commit
  enum {
    u3_saga_sync = 0,   //!< sync commit mode
    u3_saga_async,      //!< async commit mode
  } mod_e;              //!< commit mode
  c3_t         act_t;   //!< active commit flag
  u3_saga_acon asy_u;   //!< async commit context
};

//==============================================================================
// Constants
//==============================================================================

//! Name of file containing the fake bit.
static const c3_c fak_nam_c[] = "fake.bin";

//! Name of file containing event log version.
static const c3_c ver_nam_c[] = "version.bin";

//! Name of file containing the name of the ship.
static const c3_c who_nam_c[] = "who.bin";

//! Event log version number.
static const c3_w elo_ver_w = 1;

//! Max number of events per epoch.
static const size_t epo_len_i = 100;

//==============================================================================
// Static functions
//==============================================================================

//! Compare two epoch directory names. Used as the comparison function for
//! qsort().
//!
//! @param[in] lef_v  Pointer to character array representing left epoch
//!                   directory name.
//! @param[in] rih_v  Pointer to character array representing right epoch
//!                   directory name.
//!
//! @return <0  The left epoch is older than the right epoch.
//! @return  0  The left epoch and right epoch are the same age (i.e. the same
//!             epoch).
//! @return >0  The left epoch is younger than the right epoch.
static inline c3_i
_cmp_epocs(const void* lef_v, const void* rih_v);

//! Create metadata files for the fake bit and identity.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] met_u  Pier metadata.
//!
//! @return 1  Both files were created.
//! @return 0  Otherwise.
static c3_t
_create_metadata_files(const u3_saga* const log_u, const u3_meta* const met_u);

//! Determine if an epoch is full (i.e. has reached the maximum epoch length).
//!
//! @param[in] poc_u  Epoch handle.
//!
//! @return 1  Epoch is full.
//! @return 0  Otherwise.
static inline c3_t
_epoc_is_full(const u3_epoc* const poc_u);

//! Search an event log's list of epochs for the epoch that contains the given
//! event ID.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] ide_d  Event ID to search for.
//!
//! @return NULL  `ide_d` does not belong to any epoch in `log_u`.
//! @return       Epoch handle of epoch containing `ide_d`.
static u3_epoc*
_find_epoc(u3_saga* const log_u, const c3_d ide_d);

//! Determine if a string is a valid epoch directory name.
//!
//! @param[in] nam_c  Name.
//!
//! @return 1  `nam_c` is a valid epoch directory name.
//! @return 0  Otherwise.
static inline c3_t
_is_epoc_dir(const c3_c* const nam_c);

//! Migrate from old non-epoch-based event log to epoch-based event log.
//!
//! @param[in]  log_u  Event log handle.
//! @param[out] met_u  Pointer to pier metadata.
//!
//! @return 1  Migration succeeded.
//! @return 0  Otherwise.
static c3_t
_migrate(u3_saga* const log_u, u3_meta* const met_u);

//! Discover epoch directories in a given directory.
//!
//! @param[in]  dir_c  Directory to search for epoch directories.
//! @param[out] ent_c  Pointer to array of 256-byte arrays.
//! @param[out] ent_i  Pointer to number of elements in `*ent_c`.
//!
//! @return 1  Discovered one or more epoch directories.
//! @return 0  Otherwise.
static c3_t
_read_epoc_dirs(const c3_c* const dir_c, c3_c (**ent_c)[], size_t* ent_i);

//! Remove events that were committed in the last commit request from an event
//! log's pending commits list.
//!
//! @param[in] log_u  Event log handle.
static inline void
_remove_committed_events(u3_saga* const log_u);

//! Get the number of events that can be committed to an epoch.
//!
//! @param[in] poc_u  Epoch to commit to.
//! @param[in] eve_u  List of events pending commit.
//!
//! @return  Number of events that can be committed to `poc_u`.
static inline size_t
_request_len(const u3_epoc* const poc_u, const c3_list* const eve_u);

//! Update an event log's current epoch to the most recent epoch, implying that
//! the previous current epoch just became full.
//!
//! @param[in,out] log_u  Event log handle. Current epoch is updated.
//!
//! @return  Epoch handle of new current epoch.
static inline u3_epoc*
_rollover(u3_saga* const log_u);

//! Invoke user callback after batch async commit.
//!
//! @note Runs on main thread.
//!
//! @param[in] req_u  libuv work handle.
//! @param[in] sas_i  libuv return status.
static void
_uv_commit_after_cb(uv_work_t* req_u, c3_i sas_i);

//! Kick off async batch commit.
//!
//! @note Runs off main thread.
//!
//! @param[in] req_u  libuv work handle.
static void
_uv_commit_cb(uv_work_t* req_u);

static inline c3_i
_cmp_epocs(const void* lef_v, const void* rih_v)
{
  static const size_t siz_i = sizeof(((struct dirent*)NULL)->d_name);
  const c3_c*         lef_c = *(const c3_c(*)[siz_i])lef_v;
  const c3_c*         rih_c = *(const c3_c(*)[siz_i])rih_v;
  const c3_i          dif_i = strlen(lef_c) - strlen(rih_c);
  return 0 == dif_i ? strcmp(lef_c, rih_c) : dif_i;
}

static c3_t
_create_metadata_files(const u3_saga* const log_u, const u3_meta* const met_u)
{
  c3_t        suc_t = 0;
  const void* dat_v;

  c3_path_push(log_u->pax_u, fak_nam_c);
  dat_v = &met_u->fak_o;
  if ( !c3_bile_write_new(log_u->pax_u, dat_v, sizeof(met_u->fak_o)) ) {
    goto pop_path;
  }
  c3_path_pop(log_u->pax_u);

  c3_path_push(log_u->pax_u, who_nam_c);
  dat_v = met_u->who_d;
  suc_t = c3_bile_write_new(log_u->pax_u, dat_v, sizeof(met_u->who_d));

pop_path:
  c3_path_pop(log_u->pax_u);
end:
  return suc_t;
}

static inline c3_t
_is_epoc_dir(const c3_c* const nam_c)
{
  return 0 == strncmp(nam_c, epo_pre_c, strlen(epo_pre_c));
}

static inline c3_t
_epoc_is_full(const u3_epoc* const poc_u)
{
  return !u3_epoc_is_empty(poc_u)
         && 0 == u3_epoc_last_commit(poc_u) % epo_len_i;
}

//! @n (1) Push the newly created epoch from migration onto the epoch list.
//! @n (2) Immediately rollover to a new epoch so that we're not attempting to
//!        commit to the first epoch, which is almost certainly larger than the
//!        configured max epoch length.
static c3_t
_migrate(u3_saga* const log_u, u3_meta* const met_u)
{
  u3_epoc* poc_u = u3_epoc_migrate(log_u->pax_u, log_u->pax_u, met_u);
  if ( !poc_u ) {
    goto fail;
  }

  if ( !_create_metadata_files(log_u, met_u) ) {
    goto fail;
  }

  log_u->eve_d = u3_epoc_last_commit(poc_u);

  { // (1)
    try_list(log_u->epo_u.lis_u = c3_list_init(), goto fail);
    c3_list_pushb(log_u->epo_u.lis_u, poc_u, epo_siz_i);
    c3_free(poc_u);
  }

  { // (2)
    poc_u = u3_epoc_new(log_u->pax_u, log_u->eve_d + 1, 0);
    c3_list_pushb(log_u->epo_u.lis_u, poc_u, epo_siz_i);
    c3_free(poc_u);
    log_u->epo_u.cur_u = _rollover(log_u);
  }

  try_list(log_u->eve_u.lis_u = c3_list_init(), goto fail);

  goto succeed;

fail:
  return 0;

succeed:
  return 1;
}

//! @n (1) Arbitrarily choose 16 as the initial guess at the max number of
//!        epochs.
static c3_t
_read_epoc_dirs(const c3_c* const dir_c, c3_c (**ent_c)[], size_t* ent_i)
{
  DIR* dir_u;
  if ( !dir_c || !ent_c || !ent_i || !(dir_u = opendir(dir_c)) ) {
    return 0;
  }

  *ent_c = NULL;
  *ent_i = 0;

  struct dirent* ent_u;
  const size_t   siz_i = sizeof(ent_u->d_name);
  size_t         cap_i = 16; // (1)
  c3_c(*dst_c)[siz_i]  = c3_malloc(cap_i * siz_i);
  size_t dst_i         = 0;
  while ( (ent_u = readdir(dir_u)) ) {
    if ( !_is_epoc_dir(ent_u->d_name) ) {
      continue;
    }
    if ( dst_i == cap_i ) {
      cap_i *= 2;
      dst_c = c3_realloc(dst_c, cap_i * siz_i);
    }
    strcpy(dst_c[dst_i++], ent_u->d_name);
  }
  if ( 0 == dst_i ) {
    c3_free(dst_c);
    return 0;
  }
  qsort(dst_c, dst_i, siz_i, _cmp_epocs);
  *ent_c = dst_c;
  *ent_i = dst_i;
  return 1;
}

static inline void
_remove_committed_events(u3_saga* const log_u)
{
  c3_list* eve_u = log_u->eve_u.lis_u;
  size_t   len_i = log_u->eve_u.req_i;
  for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
    c3_free(c3_list_popf(eve_u));
  }
}

static inline size_t
_request_len(const u3_epoc* const poc_u, const c3_list* const eve_u)
{
  size_t len_i = u3_epoc_len(poc_u);
  c3_assert(len_i <= epo_len_i);
  size_t rem_i = epo_len_i - len_i;
  return c3_min(rem_i, c3_list_len(eve_u));
}

static inline u3_epoc*
_rollover(u3_saga* const log_u)
{
  return log_u->epo_u.cur_u = c3_lode_data(c3_list_peekb(log_u->epo_u.lis_u));
}

//! @n (1) Attempt to commit events that were enqueued after the commit began.
static void
_uv_commit_after_cb(uv_work_t* req_u, c3_i sas_i)
{
  u3_saga* log_u = req_u->data;
  log_u->act_t   = 0;

  c3_t suc_t = log_u->asy_u.suc_t;
  if ( suc_t ) {
    _remove_committed_events(log_u);
  }

  c3_d las_d = u3_epoc_last_commit(log_u->epo_u.cur_u);
  log_u->asy_u.com_f(log_u->asy_u.ptr_v, las_d, suc_t);

  if ( UV_ECANCELED != sas_i ) { // (1)
    u3_saga_commit(log_u, NULL, 0);
  }
}

static void
_uv_commit_cb(uv_work_t* req_u)
{
  u3_saga* log_u     = req_u->data;
  u3_epoc* poc_u     = log_u->epo_u.cur_u;
  c3_lode* nod_u     = c3_list_peekf(log_u->eve_u.lis_u);
  size_t   len_i     = log_u->eve_u.req_i;
  log_u->asy_u.suc_t = u3_epoc_commit(poc_u, nod_u, len_i);
}

static u3_epoc*
_find_epoc(u3_saga* const log_u, const c3_d ide_d)
{
  c3_lode* nod_u = c3_list_peekb(log_u->epo_u.lis_u);
  u3_epoc* poc_u;
  while ( nod_u ) {
    poc_u = c3_lode_data(nod_u);
    if ( u3_epoc_has(poc_u, ide_d) ) {
      break;
    }
    nod_u = c3_lode_prev(nod_u);
  }
  return nod_u ? poc_u : NULL;
}

//==============================================================================
// Functions
//==============================================================================

//! @n (1) Persist metadata.
//! @n (2) Create first epoch.
u3_saga*
u3_saga_new(const c3_path* const pax_u, const u3_meta* const met_u)
{
  u3_saga* log_u = c3_calloc(sizeof(*log_u));
  if ( !(log_u->pax_u = c3_path_fv(1, c3_path_str(pax_u))) ) {
    goto free_event_log;
  }
  mkdir(c3_path_str(log_u->pax_u), 0700);

  if ( !_create_metadata_files(log_u, met_u) ) { // (1)
    goto free_event_log;
  }

  { // (2)
    try_list(log_u->epo_u.lis_u = c3_list_init(), goto free_event_log);
    u3_epoc* poc_u;
    try_epoc(poc_u = u3_epoc_new(log_u->pax_u, epo_min_d, met_u->lif_w),
             goto free_event_log);
    c3_list_pushb(log_u->epo_u.lis_u, poc_u, epo_siz_i);
    c3_free(poc_u);
    log_u->epo_u.cur_u = c3_lode_data(c3_list_peekb(log_u->epo_u.lis_u));
  }

  try_list(log_u->eve_u.lis_u = c3_list_init(), goto free_event_log);

  goto succeed;

free_event_log:
  u3_saga_close(log_u);
  c3_free(log_u);
  return NULL;

succeed:
  return log_u;
}

//! @n (1) Attempt to migrate old non-epoch-based event log.
//! @n (2) Read metadata from filesystem.
u3_saga*
u3_saga_open(const c3_path* const pax_u, u3_meta* const met_u)
{
  u3_saga* log_u = c3_calloc(sizeof(*log_u));
  if ( !(log_u->pax_u = c3_path_fv(1, c3_path_str(pax_u))) ) {
    goto free_event_log;
  }

  { // (1)
    c3_path_push(log_u->pax_u, "data.mdb");
    c3_i ret_i = access(c3_path_str(log_u->pax_u), R_OK | W_OK);
    c3_path_pop(log_u->pax_u);
    if ( 0 == ret_i ) {
      if ( !_migrate(log_u, met_u) ) {
        goto free_event_log;
      }
      goto succeed;
    }
  }

  { // (2)
    void* dat_v;

    c3_path_push(log_u->pax_u, fak_nam_c);
    dat_v = &met_u->fak_o;
    if ( !c3_bile_read_existing(log_u->pax_u, dat_v, sizeof(met_u->fak_o)) ) {
      goto free_event_log;
    }
    c3_path_pop(log_u->pax_u);

    c3_path_push(log_u->pax_u, who_nam_c);
    dat_v = met_u->who_d;
    if ( !c3_bile_read_existing(log_u->pax_u, dat_v, sizeof(met_u->who_d)) ) {
      goto free_event_log;
    }
    c3_path_pop(log_u->pax_u);
  }

  c3_c(*ent_c)[sizeof(((struct dirent*)NULL)->d_name)];
  size_t ent_i;
  if ( !_read_epoc_dirs(c3_path_str(log_u->pax_u), &ent_c, &ent_i) ) {
    goto free_event_log;
  }

  try_list(log_u->epo_u.lis_u = c3_list_init(), goto free_dir_entries);
  for ( size_t idx_i = 0; idx_i < ent_i; idx_i++ ) {
    c3_path_push(log_u->pax_u, ent_c[idx_i]);
    u3_epoc* poc_u;
    if ( 0 == idx_i ) {
      try_epoc(poc_u = u3_epoc_open(log_u->pax_u, &met_u->lif_w),
               goto free_dir_entries);
    }
    else {
      try_epoc(poc_u = u3_epoc_open(log_u->pax_u, NULL), goto free_dir_entries);
    }
    c3_list_pushb(log_u->epo_u.lis_u, poc_u, epo_siz_i);
    c3_free(poc_u);
    c3_path_pop(log_u->pax_u);
  }
  log_u->epo_u.cur_u = c3_lode_data(c3_list_peekb(log_u->epo_u.lis_u));
  log_u->eve_d       = u3_epoc_last_commit(log_u->epo_u.cur_u);

  try_list(log_u->eve_u.lis_u = c3_list_init(), goto free_dir_entries);

  c3_free(ent_c);
  goto succeed;

free_dir_entries:
  c3_free(ent_c);
free_event_log:
  u3_saga_close(log_u);
  c3_free(log_u);
  return NULL;

succeed:
  return log_u;
}

c3_d
u3_saga_last_commit(const u3_saga* const log_u)
{
  return u3_epoc_last_commit(log_u->epo_u.cur_u);
}

//! @n (1) Bootstrap is needed if the only epoch present is the first epoch.
c3_t
u3_saga_needs_bootstrap(const u3_saga* const log_u)
{
  c3_assert(log_u);
  const u3_epoc* const poc_u = c3_lode_data(c3_list_peekf(log_u->epo_u.lis_u));
  const size_t         len_i = c3_list_len(log_u->epo_u.lis_u);
  return epo_min_d == u3_epoc_first_commit(poc_u) && 1 == len_i; // (1)
}

void
u3_saga_commit_mode(u3_saga* const log_u, u3_saga_acon* asy_u)
{
  if ( !asy_u ) {
    log_u->mod_e = u3_saga_sync;
    return;
  }

  log_u->mod_e = u3_saga_async;
  log_u->asy_u = (u3_saga_acon){
    .lup_u      = asy_u->lup_u,
    .req_u.data = log_u,
    .com_f      = asy_u->com_f,
    .ptr_v      = asy_u->ptr_v,
  };
}

//! @n (1) A NULL event can be passed to invoke another commit batch.
//! @n (2) Timing of rollover is key: a new epoch must be created when the last
//!        event to be committed to the current epoch is enqueued for commit to
//!        ensure that the snapshot captures the state of the system at the end
//!        of the current epoch, but the new epoch cannot be switched to until
//!        that enqueued event is actually committed.
c3_t
u3_saga_commit(u3_saga* const log_u, c3_y* const byt_y, const size_t byt_i)
{
  c3_list* eve_u = log_u->eve_u.lis_u;
  if ( byt_y ) { // (1)
    c3_list_pushb(eve_u, byt_y, byt_i);
    log_u->eve_d++;
  }
  else if ( 0 == c3_list_len(eve_u) ) {
    goto succeed;
  }

  c3_list* epo_u = log_u->epo_u.lis_u;
  u3_epoc* poc_u = log_u->epo_u.cur_u;
  if ( 0 == log_u->eve_d % epo_len_i ) { // (2)
    u3_epoc* new_u = u3_epoc_new(log_u->pax_u, log_u->eve_d + 1, 0);
    c3_assert(new_u);
    c3_list_pushb(epo_u, new_u, epo_siz_i);
    c3_free(new_u);
  }

  switch ( log_u->mod_e ) {
    case u3_saga_sync:
      if ( _epoc_is_full(poc_u) ) {
        poc_u = _rollover(log_u);
      }
      log_u->eve_u.req_i = _request_len(poc_u, eve_u);
      c3_lode* nod_u     = c3_list_peekf(eve_u);
      log_u->act_t       = 1;
      if ( !u3_epoc_commit(poc_u, nod_u, log_u->eve_u.req_i) ) {
        goto fail;
      }
      _remove_committed_events(log_u);
      log_u->act_t = 0;
      goto succeed;
    case u3_saga_async:
      if ( !log_u->act_t ) {
        if ( _epoc_is_full(poc_u) ) {
          poc_u = _rollover(log_u);
        }
        log_u->eve_u.req_i = _request_len(poc_u, eve_u);
        log_u->act_t       = 1;
        uv_queue_work(log_u->asy_u.lup_u,
                      &log_u->asy_u.req_u,
                      _uv_commit_cb,
                      _uv_commit_after_cb);
      }
      goto succeed;
  }

fail:
  return 0;

succeed:
  return 1;
}

//! TODO(peter): what happens if the requested replay-to event is no longer
//! present because it was in a truncated epoch?
//! @n (1) Replay by restoring the latest epoch's snapshot and then replaying
//!        that epoch's events (the default).
//! @n (2) Replay by replaying all epoch's events.
c3_t
u3_saga_replay(u3_saga* const log_u,
               c3_d           cur_d,
               c3_d           las_d,
               u3_saga_play   pla_f,
               void*          ptr_v)
{
#ifndef U3_REPLAY_FULL /* (1) */
  c3_t suc_t = 0;

  if ( 0 == las_d ) {
    las_d = u3_epoc_last_commit(log_u->epo_u.cur_u);
  }

  if ( las_d <= cur_d ) {
    suc_t = 1;
    goto end;
  }

  u3_epoc* poc_u;
  try_saga(poc_u = _find_epoc(log_u, las_d),
           goto end,
           "no epoch has event %" PRIu64,
           las_d);

  if ( !u3_epoc_is_first(poc_u) ) {
    cur_d = u3_epoc_first_evt(poc_u) <= u3A->eve_d
              ? u3A->eve_d
              : u3m_boot(u3_epoc_path_str(poc_u), u3e_load);
  }
  cur_d++;

  try_epoc(u3_epoc_iter_open(poc_u, cur_d), goto take_snapshot);
  while ( cur_d <= las_d ) {
    c3_y*  byt_y;
    size_t len_i;
    try_epoc(u3_epoc_iter_step(poc_u, &byt_y, &len_i), goto close_iterator);
    if ( !pla_f(ptr_v, cur_d, las_d, byt_y, len_i) ) {
      goto close_iterator;
    }
    cur_d++;
  }
  suc_t = 1;

close_iterator:
  u3_epoc_iter_close(poc_u);
take_snapshot:
  u3e_save();
end:
  return suc_t;
#else /* @2 */
  if ( 0 == las_d ) {
    las_d = u3_epoc_last_commit(log_u->epo_u.cur_u);
  }

  if ( las_d == cur_d ) {
    return 1;
  }

  cur_d++;

  c3_lode* nod_u = c3_list_peekf(log_u->epo_u.lis_u);
  u3_epoc* poc_u = NULL;
  c3_d     end_d = 0;
  while ( 1 ) {
    if ( !poc_u ) {
      poc_u = c3_lode_data(nod_u);
      end_d = u3_epoc_last_commit(poc_u);
      try_epoc(u3_epoc_iter_open(poc_u, cur_d), exit(9));
    }
    c3_y*  byt_y;
    size_t len_i;
    try_epoc(u3_epoc_iter_step(poc_u, &byt_y, &len_i), exit(10));
    if ( !pla_f(ptr_v, cur_d, las_d, byt_y, len_i) ) {
      return 0;
    }
    cur_d++;
    if ( las_d < cur_d ) {
      u3_epoc_iter_close(poc_u);
      break;
    }
    else if ( end_d < cur_d ) {
      u3_epoc_iter_close(poc_u);
      poc_u = NULL;
      nod_u = c3_lode_next(nod_u);
    }
  }
  return 1;
#endif
}

void
u3_saga_info(const u3_saga* const log_u)
{
  if ( !log_u ) {
    return;
  }

  fprintf(stderr,
          "\r\nsaga: last commit: %" PRIu64 "\r\n",
          u3_saga_last_commit(log_u));

  fprintf(stderr,
          "  events pending commit: %lu\r\n",
          c3_list_len(log_u->eve_u.lis_u));

  c3_lode* nod_u = c3_list_peekf(log_u->epo_u.lis_u);
  while ( nod_u ) {
    u3_epoc* poc_u = c3_lode_data(nod_u);
    u3_epoc_info(poc_u);
    nod_u = c3_lode_next(nod_u);
  }
}

//! @n (1) Cancel thread that is performing async commits.
//!        XX can deadlock from signal handler
//!        XX revise SIGTSTP handling
//! @n (2) Free epochs.
//! @n (3) Free events pending commit.
void
u3_saga_close(u3_saga* const log_u)
{
  if ( !log_u ) {
    return;
  }

  if ( u3_saga_async == log_u->mod_e && log_u->act_t ) { // (1)
    while ( UV_EBUSY == uv_cancel((uv_req_t*)&log_u->asy_u.req_u) );
  }

  if ( log_u->pax_u ) {
    c3_path_free(log_u->pax_u);
  }

  { // (2)
    c3_list* epo_u = log_u->epo_u.lis_u;
    if ( epo_u ) {
      c3_lode* nod_u;
      while ( (nod_u = c3_list_popf(epo_u)) ) {
        u3_epoc* poc_u = c3_lode_data(nod_u);
        u3_epoc_close(poc_u);
        c3_free(nod_u);
      }
      c3_free(epo_u);
    }
  }

  { // (3)
    c3_list* eve_u = log_u->eve_u.lis_u;
    if ( eve_u ) {
      for ( size_t idx_i = 0; idx_i < c3_list_len(eve_u); idx_i++ ) {
        c3_free(c3_list_popf(eve_u));
      }
      c3_free(eve_u);
    }
  }
}
