/// @file lock.h
///
/// Lockfile.

#ifndef U3_VERE_LOCK_H
#define U3_VERE_LOCK_H

#include "c/path.h"

//==============================================================================
// Functions
//==============================================================================

/// Acquire a lockfile, killing anything that holds it.
///
/// @param[in] pax_u  Directory containing lockfile.
void
u3_lock_acquire(c3_path* const pax_u);

/// Release a lockfile.
///
/// @param[in] pax_u  Directory containing lockfile.
void
u3_lock_release(c3_path* const pax_u);

#endif /* ifndef U3_VERE_LOCK_H */
