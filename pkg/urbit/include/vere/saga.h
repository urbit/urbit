//! @file saga.h
//!
//! Epoch-based event log.
//!
//! @warning Do *not* call into this module unless *all* noun references are
//!          roots. If not all noun references are roots, memory leaks will
//!          result.

#ifndef U3_VERE_EVLO_H
#define U3_VERE_EVLO_H

#include <uv.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/list.h"
#include "c/path.h"
#include "vere/epoc.h"
#include "vere/meta.h"

//==============================================================================
// Types
//==============================================================================

//! User-provided function to replay an event.
//!
//! @param[in] ptr_v  Context provided to u3_saga_replay() as `ptr_v`.
//! @param[in] cur_d  ID of current event in replay.
//! @param[in] las_d  ID of last event to be replayed.
//! @param[in] byt_y  Serialized event.
//! @param[in] byt_i  Length of `byt_y` in bytes.
//!
//! @return 1  Event replay succeeded.
//! @return 0  Otherwise.
typedef c3_t (*u3_saga_play)(void*        ptr_v,
                             c3_d         cur_d,
                             c3_d         las_d,
                             c3_y* const  byt_y,
                             const size_t byt_i);

//! User-provided function to execute on main thread after an async batch commit
//! has completed.
//!
//! @param[in] ptr_v  Context provided to u3_saga_commit_async() as `ptr_v`.
//! @param[in] ide_d  ID of last event committed in async batch commit.
//! @param[in] suc_t  True if the commit succeeded.
//!
//! @return 1  Success.
//! @return 0  Otherwise.
typedef c3_t (*u3_saga_news)(void* ptr_v, c3_d ide_d, c3_t suc_t);

//! Event log.
struct _u3_saga;
typedef struct _u3_saga u3_saga;

//==============================================================================
// Constants
//==============================================================================

//! Event log version number.
extern const c3_w elo_ver_w;

//==============================================================================
// Macros
//==============================================================================

//! Error handling wrapper for u3_saga API calls that return 0/NULL on failure
//! and non-zero/non-NULL on success.
//!
//! @param[in] saga_call       u3_saga API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_saga(saga_call, failure_action, ...)                               \
  do {                                                                         \
    if ( !(saga_call) ) {                                                      \
      fprintf(stderr, "saga: " __VA_ARGS__);                                   \
      fprintf(stderr, "\r\n");                                                 \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Functions
//==============================================================================

//! Create a new event log.
//!
//! @param[in] pax_u  Root directory of event log. Will be created if it
//!                   doesn't already exist.
//!
//! @return NULL  New event log could not be created.
//! @return       Handle to new event log.
u3_saga*
u3_saga_new(const c3_path* const pax_u);

//! Load an existing event log created with u3_saga_new().
//!
//! TODO: explain assumptions around u3m_init().
//!
//! @param[in]  pax_u  Root directory of event log.
//! @param[out] len_w  Pointer to boot sequence length. If set to 0, this
//!                    indicates that the first epoch has been truncated.
//!
//! @return NULL  Existing event log could not be opened.
//! @return       Handle to open event log.
u3_saga*
u3_saga_open(const c3_path* const pax_u, c3_w* const len_w);

//! Get the ID of the last committed event in an event log.
//!
//! @param[in] log_u  Event log handle. Must not be NULL.
//!
//! @return  ID of last committed event in `log_u`.
c3_d
u3_saga_last_commit(const u3_saga* const log_u);

//! Determine if a new epoch should be created. Does NOT create a new epoch. To
//! do so, call u3_saga_rollover().
//!
//! @param[in] log_u  Event log handle. Must not be NULL.
//!
//! @return 1  A new epoch should be created for `log_u`.
//! @return 0  Otherwise.
c3_t
u3_saga_needs_rollover(const u3_saga* const log_u);

//! Determine if the bootstrap sequence is needed to replay an event log.
//!
//! @param[in] log_u  Event log handle. Must not be NULL.
//!
//! @return 1  `log_u` requires the boostrap sequence for replay.
//! @return 0  Otherwise.
c3_t
u3_saga_needs_bootstrap(const u3_saga* const log_u);

//! Synchronously commit an event to an event log.
//!
//! Blocks until the event has been committed.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] byt_y  Serialized event.
//! @param[in] byt_i  Length of `byt_y` in byts.
//!
//! @return 1  Event was committed.
//! @return 0  Otherwise.
c3_t
u3_saga_commit_sync(u3_saga* const log_u, c3_y* const byt_y, const size_t byt_i);

//! Set the context for an asynchronous commit.
//!
//! This MUST be called before u3_saga_commit_async().
//!
//! @param[in] log_u  Event log handle.
//! @param[in] lup_u  libuv event loop.
//! @param[in] com_f  Callback invoked upon commit completion.
//! @param[in] ptr_v  User context passed to `com_f`.
void
u3_saga_set_async_ctx(u3_saga* const log_u,
                      uv_loop_t* const lup_u,
                      u3_saga_news com_f,
                      void* ptr_v);

//! Asynchronously commit an event to an event log.
//!
//! Returns immediately and invokes the user-provided callback on the main
//! thread once the event is committed.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] byt_y  Serialized event.
//! @param[in] byt_i  Length of `byt_y` in bytes.
//!
//! @return 1  Event was scheduled for commit.
//! @return 0  Otherwise.
c3_t
u3_saga_commit_async(u3_saga* const log_u,
                     c3_y* const byt_y,
                     const size_t byt_i);

//! Roll an event log over to a new epoch. Future calls to u3_saga_commit()
//! will commit to this new epoch.
//!
//! @param[in] log_u  Event log handle.
//!
//! @return 1  New epoch was successfully created.
//! @return 0  Otherwise.
c3_t
u3_saga_rollover(u3_saga* const log_u);

//! Truncate the event log by deleting old epochs.
//!
//! @param[in] cnt_i  Number of old epochs to remove. If 0 or if greater
//!                   than or equal to the current number of epochs, all but the
//!                   most recent epoch will be deleted
//!
//! @return 1  Old epochs successfully deleted.
//! @return 0  Otherwise.
c3_t
u3_saga_truncate(u3_saga* const log_u, size_t cnt_i);

//! Replay part or all of an event log.
//!
//! TODO: explain assumptions around u3m_init().
//!
//! @param[in] log_u  Event log handle.
//! @param[in] cur_d  ID of event last applied.
//! @param[in] las_d  ID of event to end replay at (inclusive). Must be greater
//!                   than or equal to the ID of the first committed event in
//!                   the most recent epoch. If 0, all remaining committed
//!                   events will be replayed.
//! @param[in] pla_f  Replay function invoked on each event.
//! @param[in] ptr_v  Context passed to replay function.
//!
//! @return 1  Replay succeeded.
//! @return 0  Otherwise.
c3_t
u3_saga_replay(u3_saga* const log_u,
               c3_d           cur_d,
               c3_d           las_d,
               u3_saga_play   pla_f,
               void*          ptr_v);

//! Print status info about an event log.
//!
//! @param[in]  Event log handle.
void
u3_saga_info(const u3_saga* const log_u);

//! Gracefully dispose of an event log's resources. Does not free the event log
//! handle itself.
//!
//! @param[in] log_u  Event log handle.
void
u3_saga_close(u3_saga* const log_u);

#endif /* ifndef U3_VERE_EVLO_H */
