//! @file epoc.h
//! Event log epoch containing a snapshot representing the state before the
//! application of the first event in the epoch along with an LMDB instance
//! containing all events in the epoch.
//!
//! As an example, the directory layout for epoch `0i100` is:
//! ```console
//! 0i100/
//!   data.mdb
//!   lock.mdb
//!   north.bin
//!   south.bin
//! ```

#ifndef U3_VERE_EPOC_H
#define U3_VERE_EPOC_H

#include <lmdb.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/list.h"
#include "c/path.h"
#include "vere/meta.h"

//==============================================================================
// Types
//==============================================================================

//! Iterator over an epoch's events.
//!
//! Must only ever be created via u3_epoc_iter_open() and managed via
//! u3_epoc_iter_step() and u3_epoc_iter_close().
typedef struct {
  c3_t        ope_t; //!< true if iterator is open
  MDB_txn*    txn_u; //!< LMDB read-only transaction
  MDB_dbi     dbi_u; //!< LMDB database handle
  MDB_cursor* cur_u; //!< LMDB cursor
  c3_d        fir_d; //!< starting event ID of iterator
  c3_d        cur_d; //!< current event ID of iterator
} u3_epoc_iter;

//! Epoch handle.
//!
//! Must only ever be created via u3_epoc_new() or u3_epoc_open() and disposed
//! of via u3_epoc_free(). Fields can be accessed directly or via the
//! u3_epoc_*() getter methods below depending on which is more convenient.
typedef struct {
  c3_path*     pax_u; //!< path to epoch directory
  MDB_env*     env_u; //!< LMDB environment
  c3_d         fir_d; //!< ID of first committed event
  c3_d         las_d; //!< ID of last committed event
  u3_epoc_iter itr_u; //!< iterator
} u3_epoc;

//==============================================================================
// Constants
//==============================================================================

//! Prefix of epoch directory name.
extern const c3_c epo_pre_c[];

//! Smallest event ID that an epoch will accept.
extern const c3_d epo_min_d;

//==============================================================================
// Macros
//==============================================================================

//! Error handling wrapper for u3_epoc API calls that return 0/NULL on failure
//! and non-zero/non-NULL on success.
//!
//! @param[in] epoc_call       u3_epoc API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_epoc(epoc_call, failure_action, ...)                               \
  do {                                                                         \
    if ( !(epoc_call) ) {                                                      \
      fprintf(stderr, "epoc: " __VA_ARGS__);                                   \
      fprintf(stderr, "\r\n");                                                 \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Functions
//==============================================================================

//! Get the path of an epoch.
static inline const c3_c*
u3_epoc_path(const u3_epoc* const poc_u)
{
  return c3_path_str(poc_u->pax_u);
}

//! Get the ID of the first committed event in an epoch.
static inline c3_d
u3_epoc_first_commit(const u3_epoc* const poc_u)
{
  return poc_u->fir_d;
}

//! Get the ID of the last committed event in an epoch.
static inline c3_d
u3_epoc_last_commit(const u3_epoc* const poc_u)
{
  return poc_u->las_d;
}

//! Get the ID of the first event represented by the epoch, which is equivalent
//! to the ID of the event that corresponds to the epoch's snapshot.
static inline c3_d
u3_epoc_first_evt(const u3_epoc* const poc_u)
{
  return u3_epoc_first_commit(poc_u) - 1;
}

static inline c3_t
u3_epoc_is_empty(const u3_epoc* const poc_u)
{
  return poc_u->las_d < poc_u->fir_d;
}

//! Get the number of committed events in an epoch.
static inline size_t
u3_epoc_len(const u3_epoc* const poc_u)
{
  return u3_epoc_is_empty(poc_u) ? 0 : poc_u->las_d + 1 - poc_u->fir_d;
}

//! True if an epoch contains the event with given ID, false otherwise.
static inline c3_t
u3_epoc_has(const u3_epoc* const poc_u, const c3_d ide_d)
{
  return u3_epoc_first_commit(poc_u) <= ide_d
         && ide_d <= u3_epoc_last_commit(poc_u);
}

//! True if an epoch contains the smallest possible event ID, false otherwise.
static inline c3_t
u3_epoc_is_first(const u3_epoc* const poc_u)
{
  return u3_epoc_has(poc_u, epo_min_d);
}

//! Create a new empty epoch rooted at `<par_c>/0i<fir_d>`.
//!
//! @param[in] par_u  Parent directory to house epoch. Will be created if it
//!                   doesn't already exist.
//! @param[in] fir_d  Event ID of first event in epoch.
//! @param[in] lif_w  Lifecycle length of boot sequence. Only relevant for first
//!                   epoch.
u3_epoc*
u3_epoc_new(const c3_path* const par_u, const c3_d fir_d, c3_w lif_w);

//! Create a new first epoch from an existing (non-epoch-based) event log.
//!
//! @note This is intended for use in migration from the non-epoch-based event
//!       log to the new epoch-based event log.
//!
//! @param[in]  par_u  Parent directory to house epoch. Will be created if it
//!                    doesn't already exist.
//! @param[in]  src_u  Path to directory housing existing event log.
//! @param[out] met_u  Pointer to pier metadata.
u3_epoc*
u3_epoc_migrate(const c3_path* const par_u,
                c3_path* const       src_u,
                u3_meta* const       met_u);

//! Load an existing epoch created with u3_epoc_new().
//!
//! @param[in]  pax_u  Root directory of epoch.
//! @param[out] lif_w  Pointer to length of boot sequence. Only relevant for
//!                    first epoch. Can be NULL if not first epoch.
u3_epoc*
u3_epoc_open(const c3_path* const pax_u, c3_w* const lif_w);

//! Commit one or more serialized events to an epoch's LMDB instance.
//!
//! @param[in] poc_u  Epoch handle.
//! @param[in] nod_u  c3_list node of first event to commit with serialized
//!                   event as payload.
//! @param[in] len_i  Number of events to commit.
c3_t
u3_epoc_commit(u3_epoc* const poc_u, const c3_lode* nod_u, const size_t len_i);

//! Open an iterator within an epoch.
//!
//! @param[in] poc_u  Epoch handle.
//! @param[in] ide_d  Event ID to initilize iterator at.
//!
//! @return 0  `poc_u` already has an open iterator.
//! @return 0  `poc_u` doesn't contain event ID `ide_d`.
c3_t
u3_epoc_iter_open(u3_epoc* const poc_u, c3_d ide_d);

//! Get the event at the current iterator and advance the iterator to the next
//! event.
//!
//! @param[in]  poc_u  Must not be NULL.
//! @param[out] byt_y  Pointer to hold serialized event buffer. Must not be
//!                    NULL. `*byt_y` must NOT be freed.
//! @param[out] len_i  Pointer to hold `byt_y` in bytes. Must not be NULL.
//!
//! @return 0  `poc_u` doesn't have an open iterator.
//! @return 0  Already consumed last event in `poc_u`.
c3_t
u3_epoc_iter_step(u3_epoc* const poc_u,
                  c3_y** const   byt_y,
                  size_t* const  len_i);

//! Close an epoch's iterator.
//!
//! @param[in] poc_u  Epoch handle.
void
u3_epoc_iter_close(u3_epoc* const poc_u);

//! Print info about an epoch's LMDB instance.
void
u3_epoc_info(const u3_epoc* const poc_u);

//! Gracefully dispose of an epoch's resources. Does not free the epoch handle
//! itself.
//!
//! @param[in] poc_u  Epoch handle.
void
u3_epoc_close(u3_epoc* const poc_u);

#endif /* ifndef U3_VERE_EPOC_H */
