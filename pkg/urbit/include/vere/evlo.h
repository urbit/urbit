//! @file evlo.h
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
//! @param[in] ptr_v  Context provided to u3_evlo_replay() as `ptr_v`.
//! @param[in] cur_d  ID of current event in replay.
//! @param[in] las_d  ID of last event to be replayed.
//! @param[in] byt_y  Serialized event.
//! @param[in] len_i  Length of `byt_y` in bytes.
//!
//! @return 0  Event replay failed.
//! @return 1  Event replay succeeded.
typedef c3_t (*u3_evlo_play)(void*        ptr_v,
                             c3_d         cur_d,
                             c3_d         las_d,
                             c3_y* const  byt_y,
                             const size_t len_i);

//! User-provided function to execute on main thread after an async batch commit
//! has completed.
//!
//! @param[in] ptr_v  Context provided to u3_evlo_commit_mode() as `ptr_v`.
//! @param[in] suc_d  ID of last event committed in async batch commit.
//! @param[in] suc_t  True if the commit succeeded.
typedef c3_t (*u3_evlo_news)(void* ptr_v, c3_d ide_d, c3_t suc_t);

//! Event log async commit mode context.
typedef struct {
  uv_loop_t*   lup_u; //!< libuv event loop
  uv_work_t    req_u; //!< libuv work queue handle
  u3_evlo_news com_f; //!< callback invoked upon commit completion
  void*        ptr_v; //!< user context passed to `com_f`
  c3_t         suc_t; //!< commit success flag
} u3_evlo_acon;

//! Event log handle.
typedef struct {
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
    u3_evlo_sync = 0,   //!< sync commit mode
    u3_evlo_async,      //!< async commit mode
  } mod_e;              //!< commit mode
  c3_t         act_t;   //!< active commit flag
  u3_evlo_acon asy_u;   //!< async commit context
} u3_evlo;

//==============================================================================
// Macros
//==============================================================================

//! Error handling wrapper for u3_evlo API calls that return 0/NULL on failure
//! and non-zero/non-NULL on success.
//!
//! @param[in] evlo_call       u3_evlo API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_evlo(evlo_call, failure_action, ...)                               \
  do {                                                                         \
    if ( !(evlo_call) ) {                                                      \
      fprintf(stderr, "evlo: " __VA_ARGS__);                                   \
      fprintf(stderr, "\r\n");                                                 \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Functions
//==============================================================================

static inline c3_d
u3_evlo_first_commit(const u3_evlo* const log_u)
{
  return epo_min_d;
}

static inline c3_d
u3_evlo_last_commit(const u3_evlo* const log_u)
{
  return u3_epoc_last_commit(log_u->epo_u.cur_u);
}

//! Create a new event log.
//!
//! @param[in] pax_u  Root directory of event log. Will be created if it
//!                   doesn't already exist.
//! @param[in] met_u  Pier metadata.
u3_evlo*
u3_evlo_new(const c3_path* const pax_u, const u3_meta* const met_u);

//! Load an existing event log created with u3_evlo_new().
//!
//! @param[in]  pax_u  Root directory of event log.
//! @param[out] met_u  Pier metadata.
u3_evlo*
u3_evlo_open(const c3_path* const pax_u, u3_meta* const met_u);

//! Set the commit mode of an event log to synchronous (the default) or
//! asynchronous. Must not be called when async commits are in progress.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] asy_u  Async context to use for async commits. If NULL, sets
//!                   commit mode to synchronous.
void
u3_evlo_commit_mode(u3_evlo* const log_u, u3_evlo_acon* asy_u);

//! Commit an event to an event log.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] byt_y  Serialized event.
//! @param[in] byt_i  Length of `byt_y` in bytes.
c3_t
u3_evlo_commit(u3_evlo* const log_u, c3_y* const byt_y, const size_t byt_i);

//! Replay part or all of an event log.
//!
//! @param[in] log_u  Event log handle.
//! @param[in] cur_d  ID of event last applied.
//! @param[in] las_d  ID of event to end replay at (inclusive). If 0, all
//!                   remaining committed events will be replayed.
//! @param[in] pla_f  Replay function invoked on each event.
//! @param[in] ptr_v  Context passed to replay function.
c3_t
u3_evlo_replay(u3_evlo* const log_u,
               c3_d           cur_d,
               c3_d           las_d,
               u3_evlo_play   pla_f,
               void*          ptr_v);

//! Print status info about an event log.
//!
//! @param[in]  Event log handle.
void
u3_evlo_info(const u3_evlo* const log_u);

//! Gracefully dispose of an event log's resources. Does not free the event log
//! handle itself.
//!
//! @param[in] log_u  Event log handle.
void
u3_evlo_close(u3_evlo* const log_u);

#endif /* ifndef U3_VERE_EVLO_H */
