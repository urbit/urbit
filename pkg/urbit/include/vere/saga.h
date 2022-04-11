//! @file saga.h
//!
//! Event log.
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
//! @param[in] len_i  Length of `byt_y` in bytes.
//!
//! @return 1  Event replay succeeded.
//! @return 0  Otherwise.
typedef c3_t (*u3_saga_play)(void*        ptr_v,
                             c3_d         cur_d,
                             c3_d         las_d,
                             c3_y* const  byt_y,
                             const size_t len_i);

//! User-provided function to execute on main thread after an async batch commit
//! has completed.
//!
//! @param[in] ptr_v  Context provided to u3_saga_commit_mode() as `ptr_v`.
//! @param[in] suc_d  ID of last event committed in async batch commit.
//! @param[in] suc_t  True if the commit succeeded.
//!
//! @return 1  Success.
//! @return 0  Otherwise.
typedef c3_t (*u3_saga_news)(void* ptr_v, c3_d ide_d, c3_t suc_t);

//! Event log async commit mode context.
typedef struct {
  uv_loop_t*   lup_u; //!< libuv event loop
  uv_work_t    req_u; //!< libuv work queue handle
  u3_saga_news com_f; //!< callback invoked upon commit completion
  void*        ptr_v; //!< user context passed to `com_f`
  c3_t         suc_t; //!< commit success flag
} u3_saga_acon;

//! Event log.
struct _u3_saga;
typedef struct _u3_saga u3_saga;

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
//! @param[in] met_u  Pier metadata.
//!
//! @return NULL  New event log could not be created.
//! @return       Handle to new event log.
u3_saga*
u3_saga_new(const c3_path* const pax_u, const u3_meta* const met_u);

//! Load an existing event log created with u3_saga_new().
//!
//! @param[in]  pax_u  Root directory of event log.
//! @param[out] met_u  Pier metadata.
//!
//! @return NULL  Existing event log could not be opened.
//! @return       Handle to open event log.
u3_saga*
u3_saga_open(const c3_path* const pax_u, u3_meta* const met_u);

//! Get the ID of the last committed event in an event log.
//!
//! @param[in] log_u  Event log handle. Must not be NULL.
//!
//! @return  ID of last committed event in `log_u`.
c3_d
u3_saga_last_commit(const u3_saga* const log_u);

//! Set the commit mode of an event log to synchronous (the default) or
//! asynchronous. Must not be called when async commits are in progress.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] asy_u  Async context to use for async commits. If NULL, sets
//!                   commit mode to synchronous.
void
u3_saga_commit_mode(u3_saga* const log_u, u3_saga_acon* asy_u);

//! Commit an event to an event log.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] byt_y  Serialized event.
//! @param[in] byt_i  Length of `byt_y` in bytes.
//!
//! @return 1  Event was committed (sync commit mode) or scheduled (async commit
//!            mode).
//! @return 0  Otherwise.
c3_t
u3_saga_commit(u3_saga* const log_u, c3_y* const byt_y, const size_t byt_i);

//! Replay part or all of an event log.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] cur_d  ID of event last applied.
//! @param[in] las_d  ID of event to end replay at (inclusive). If 0, all
//!                   remaining committed events will be replayed.
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
