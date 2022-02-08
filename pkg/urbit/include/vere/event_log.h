//! @file event_log.h
//! Persistent, replayable event log of nouns.

#ifndef VERE_EVENT_LOG_H
#define VERE_EVENT_LOG_H

#include "c/portable.h"
#include "c/types.h"

#include "noun/aliases.h"


//! Opaque event log type. `struct _evtlog` should be defined in the source file
//! that implements this interface.
typedef struct _evtlog u3_evtlog;


//! Create/open an event log rooted at `dir_c`.
//!
//! @param[in] dir_c  Directory containing the event log. If the directory
//!                   doesn't already exist, it will be created.
//!
//! @return NULL  `dir_c` could not be created.
//! @return NULL  Event log could not be created/opened.
//! @return       Open event log.
u3_evtlog*
u3_evtlog_init(const c3_c* const dir_c);


//! Append `eve` to the end of `log_u` without writing to non-volatile
//! memory.
//!
//! @param[in] log_u
//! @param[in] eve    Event to append.
//!
//! @return -1  `eve` could not be appended.
//! @return 0   `eve` was successfully appended.
c3_ws
u3_evtlog_append(u3_evtlog* const log_u, u3_noun eve);


//! Commit the contents of `log_u` to non-volatile memory.
//!
//! Blocks until all uncommitted events have been committed.
//!
//! @param[in] log_u
//!
//! @return -1  `log_u` contents could not be committed.
//! @return 0   `log_u` contents were successfully committed.
c3_ws
u3_evtlog_commit(u3_evtlog* const log_u);


//! Replay an arbitrarily-sized batch of events in `log_u`.
//!
//! Blocks until all events in the batch have been replayed. This function is
//! stateful in the sense that a given call will start replaying events where
//! its preceding call left off. Once the end of `log_u` has been reached, a
//! subsequent call to this function will start replay at the first event in
//! `log_u`. For example, consider an event log `log_u` with 700 events:
//!
//! ```c
//! void replay_event(const u3_noun eve) {
//!   // Replay single event.
//! }
//!
//! void replay_event_log(u3_evtlog* const log_u) {
//!   // Replay events 1-500.
//!   c3_w lef_w = u3_evtlog_replay(log_u, replay_event, 500);
//!   c3_assert(200 == lef_w);
//!
//!   // Replay events 501-700.
//!   lef_w = u3_evtlog_replay(log_u, replay_event, 500);
//!   c3_assert(0 == lef_w);
//!
//!   // Replay events 1-500 again.
//!   lef_w = u3_evtlog_replay(log_u, replay_event, 500);
//!   c3_assert(200 == lef_w);
//! }
//! ```
//!
//! @param[in] log_u
//! @param[in] fun_f  Function use to replay a single event.
//! @param[in] siz_i  Number of events in the batch. If 0 or if larger than the
//!                   number of remaining events in `log_u`, all events from the
//!                   current position in `log_u` to the end of `log_u` will be
//!                   replayed.
//!
//! @return -1  `log_u` was NULL.
//! @return -1  `fun_f` was NULL.
//! @return -1  Replay failed for another reason.
//! @return     Number of events left to replay in `log_u`.
c3_ws
u3_evtlog_replay(u3_evtlog* const log_u,
                 void (*const fun_f)(const u3_noun),
                 const size_t siz_i);


//! Free the resources associated with `log_u`.
//!
//! @param[in] log_u  If NULL, no action is taken.
void
u3_evtlog_free(u3_evtlog* const log_u);

#endif /* ifndef VERE_EVENT_LOG_H */
