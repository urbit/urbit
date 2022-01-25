//! @file time.h
//! Urbit time: 128 bits, leap-free.
//!
//! High 64 bits: 0x8000.000c.cea3.5380 + Unix time at leap 25 (Jul 2012)
//! Low 64 bits: 1/2^64 of a second.
//!
//! Seconds per Gregorian 400-block: 12.622.780.800
//! 400-blocks from 0 to 0AD: 730.692.561
//! Years from 0 to 0AD: 292.277.024.400
//! Seconds from 0 to 0AD: 9.223.372.029.693.628.800
//! Seconds between 0A and Unix epoch: 62.167.219.200
//! Seconds before Unix epoch: 9.223.372.091.860.848.000
//! The same, in C hex notation: 0x8000000cce9e0d80ULL
//!
//! XX: needs to be adjusted to implement Google leap-smear time.

#ifndef U3_VERE_TIME_H
#define U3_VERE_TIME_H

#include "c/portable.h"
#include "c/types.h"

//! Urbit seconds from unix time.
//! Adjust (externally) for future leap secs!
c3_d
u3_time_sec_in(c3_w unx_w);

//! Unix time from urbit seconds.
//! Adjust (externally) for future leap secs!
c3_w
u3_time_sec_out(c3_d urs_d);

//! Urbit fracto-seconds from unix microseconds.
c3_d
u3_time_fsc_in(c3_w usc_w);

//! Unix microseconds from urbit fracto-seconds.
c3_w
u3_time_fsc_out(c3_d ufc_d);

//! Urbit time from struct timeval.
u3_atom
u3_time_in_tv(struct timeval* tim_tv);

//! Struct timeval from urbit time.
void
u3_time_out_tv(struct timeval* tim_tv, u3_noun now);

//! Urbit time from struct timespec.
u3_atom
u3_time_in_ts(struct timespec* tim_ts);

#if defined(U3_OS_linux) || defined(U3_OS_mingw)
//! Urbit time from time_t.
u3_atom
u3_time_t_in_ts(time_t tim);
#endif

//! Struct timespec from urbit time.
void
u3_time_out_ts(struct timespec* tim_ts, u3_noun now);

//! (wen - now) in ms.
c3_d
u3_time_gap_ms(u3_noun now, u3_noun wen);

#endif /* ifndef U3_VERE_TIME_H */
