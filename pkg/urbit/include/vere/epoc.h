/// @file epoc.h
///
/// Event log epoch.
///
/// An epoch abstracts over the storage method for the events and snapshot of a
/// subset of the event log as well as the access pattern for events.
///
/// This interface is designed to be shared by all epoch implementations and to
/// be used by an implementation of the `saga` interface.
///
/// @note There is no function for asynchronous commit by design. The event log
///       implementation that uses this interface can implement asynchronous
///       event log commits if it chooses, but removing the concept of
///       asynchronicity from this interface simplifies its implementations.

#ifndef U3_VERE_EPOC_H
#define U3_VERE_EPOC_H

#include <lmdb.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/list.h"
#include "c/path.h"

// Uncomment if running tests in epoc_tests.c.
//#define U3_EPOC_TEST

//==============================================================================
// Types
//==============================================================================

/// Iterator over an epoch's events.
struct _u3_epoc_iter;
typedef struct _u3_epoc_iter u3_epoc_iter;

/// Epoch.
struct _u3_epoc;
typedef struct _u3_epoc u3_epoc;

//==============================================================================
// Constants
//==============================================================================

/// Current epoch version number.
extern const c3_w epo_ver_w;

/// Size of the `u3_epoc` type.
extern const size_t epo_siz_i;

/// Prefix of epoch directory name.
extern const c3_c epo_pre_c[];

/// Smallest event ID that an epoch will accept.
extern const c3_d epo_min_d;

//==============================================================================
// Macros
//==============================================================================

/// Error handling wrapper for u3_epoc API calls that return 0/NULL on failure
/// and non-zero/non-NULL on success.
///
/// @param[in] epoc_call       u3_epoc API call.
/// @param[in] failure_action  Statement to execute after logging failure.
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

/// Create a new empty epoch rooted at `<par_c>/0i<fir_d>`.
///
/// @param[in] par_u  Parent directory to house epoch. Will be created if it
///                   doesn't already exist.
/// @param[in] fir_d  Event ID of first event in epoch.
///
/// @return NULL  New epoch could not be created.
/// @return       Handle to new epoch.
u3_epoc*
u3_epoc_new(const c3_path* const par_u, const c3_d fir_d);

/// Create a new first epoch from an existing (non-epoch-based) event log.
///
/// @note This is intended for use in migration from the non-epoch-based event
///       log to the new epoch-based event log.
///
/// @param[in]  src_u  Path to directory housing existing event log.
/// @param[in]  par_u  Parent directory to house epoch. Will be created if it
///                    doesn't already exist.
/// @param[in]  cur_d  Event number of most recently applied event.
///
/// @return NULL  New epoch could not be created from old event log.
/// @return       Handle to migrated epoch.
u3_epoc*
u3_epoc_migrate(c3_path* const       src_u,
                const c3_path* const par_u,
                const c3_d           cur_d);

/// Load an existing epoch created with u3_epoc_new().
///
/// @param[in]  pax_u  Root directory of epoch.
/// @param[in]  rdo_t  If non-zero, the epoch will be inert (a strict form of
///                    read-only), meaning that only the epoch's first and last
///                    event numbers and path will be accessible. In particular,
///                    u3_epoc_commit() and u3_epoc_iter_*() must NOT be called.
/// @param[out] len_w  Pointer to length of boot sequence. Only relevant for
///                    first epoch. Can be NULL if not first epoch. If not NULL
///                    and not first epoch, will be set to 0.
///
/// @return NULL  Existing epoch could not be opened.
/// @return       Handle to open epoch.
u3_epoc*
u3_epoc_open(const c3_path* const pax_u, const c3_t rdo_t, c3_w* const len_w);

/// Get the file path of an epoch.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return  File path.
const c3_path*
u3_epoc_path(const u3_epoc* const poc_u);

/// Get the string representation of an epoch's file path.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return  String representation of file path.
const c3_c*
u3_epoc_path_str(const u3_epoc* const poc_u);

/// Get the ID of the first committed event in an epoch.
///
/// @param[in] poc_u  Epoch handle. Must have at least one committed event and
///                   must not be NULL.
///
/// @return  ID of epoch's first committed event.
c3_d
u3_epoc_first_commit(const u3_epoc* const poc_u);

/// Get the ID of the last committed event in an epoch.
///
/// @param[in] poc_u  Epoch handle. Must have at least one committed event and
///                   must not be NULL.
///
/// @return  ID of epoch's last committed event.
c3_d
u3_epoc_last_commit(const u3_epoc* const poc_u);

/// Get an epoch's number.
///
/// An epoch's number is equivalent to the ID of the event that corresponds to
/// the epoch's snapshot.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return  Epoch number.
c3_d
u3_epoc_num(const u3_epoc* const poc_u);

/// Determine whether an epoch has been committed to.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return 0  No events have been committed to `poc_u`.
/// @return 1  Events have been committed to `poc_u`.
c3_t
u3_epoc_is_empty(const u3_epoc* const poc_u);

/// Get the number of committed events in an epoch.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return  Number of committed events.
size_t
u3_epoc_len(const u3_epoc* const poc_u);

/// Determine if an event ID is contained within an epoch.
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
/// @param[in] ide_d  Event ID.
///
/// @return 0  `ide_d` is not in `poc_u`.
/// @return 1  `ide_d` is in `poc_u`.
c3_t
u3_epoc_has(const u3_epoc* const poc_u, const c3_d ide_d);

/// Determine if an epoch contains the smallest possible event ID (i.e. is the
/// first epoch).
///
/// @param[in] poc_u  Epoch handle. Must not be NULL.
///
/// @return 0  `poc_u` is not the first epoch.
/// @return 1  `poc_u` is the first epoch.
c3_t
u3_epoc_is_first(const u3_epoc* const poc_u);

/// Synchronously commit one or more serialized events to an epoch.
///
/// @param[in] poc_u  Epoch handle.
/// @param[in] nod_u  c3_list node (from list with `C3_LIST_TRANSFER` semantics)
///                   of first event to commit with a reference to a serialized
///                   event as a payload.
/// @param[in] len_i  Number of events to commit.
///
/// @return 0  Event could not be committed to epoc.
/// @return 1  Otherwise.
c3_t
u3_epoc_commit(u3_epoc* const poc_u, const c3_lode* nod_u, const size_t len_i);

/// Open an iterator within an epoch.
///
/// @param[in] poc_u  Epoch handle.
/// @param[in] ide_d  Event ID to initilize iterator at.
///
/// @return 0  `poc_u` already has an open iterator.
/// @return 0  `poc_u` doesn't contain event ID `ide_d`.
c3_t
u3_epoc_iter_open(u3_epoc* const poc_u, c3_d ide_d);

/// Get the event at the current iterator and advance the iterator to the next
/// event.
///
/// @param[in]  poc_u  Must not be NULL.
/// @param[out] byt_y  Pointer to hold serialized event buffer. Must not be
///                    NULL. `*byt_y` must NOT be freed.
/// @param[out] byt_i  Pointer to hold `byt_y` in bytes. Must not be NULL.
///
/// @return 0  `poc_u` doesn't have an open iterator.
/// @return 0  Already consumed last event in `poc_u`.
/// @return 1  Successfully got an event and stepped to the next.
c3_t
u3_epoc_iter_step(u3_epoc* const poc_u,
                  c3_y** const   byt_y,
                  size_t* const  byt_i);

/// Close an epoch's iterator.
///
/// @param[in] poc_u  Epoch handle.
void
u3_epoc_iter_close(u3_epoc* const poc_u);

/// Print info about an epoch's LMDB instance.
///
/// @param[in] poc_u  Epoch handle.
void
u3_epoc_info(const u3_epoc* const poc_u);

/// Gracefully dispose of an epoch's resources. Does not free the epoch handle
/// itself.
///
/// @param[in] poc_u  Epoch handle.
void
u3_epoc_close(u3_epoc* const poc_u);

/// Delete an epoch from the file system.
///
/// Does not dispose of an epoch's resources. To do so, call
/// u3_epoc_close() and then free the epoch handle.
///
/// @param[in] poc_u  Epoch handle.
///
/// @return 1  Epoch was successfully deleted from the file system.
/// @return 0  Otherwise.
c3_t
u3_epoc_delete(u3_epoc* const poc_u);

#endif /* ifndef U3_VERE_EPOC_H */
