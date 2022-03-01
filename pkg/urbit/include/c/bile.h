//! @file bile.h
//!
//! File containing a single blob of arbitrary binary data up to 2^32 bytes in
//! size.
//!
//! @note mnemonic: [b]inary f[ile] -> bile

#ifndef C3_BILE_H
#define C3_BILE_H

#include "c/portable.h"
#include "c/types.h"
#include "c/path.h"

//==============================================================================
// Types
//==============================================================================

//! Binary file handle.
//!
//! Must only be created via c3_bile_open() and disposed of via
//! c3_bile_close().
typedef struct {
  c3_i     fid_i; //!< file descriptor
  size_t   len_i; //!< length of file in bytes
  size_t   off_i; //!< offset into file that file descriptor is at
  c3_path* pax_u; //!< path to file
} c3_bile;

//==============================================================================
// Functions
//==============================================================================

//! Open/create a binary file.
//!
//! @param[in] pax_u  Path handle of binary file.
//!
//! @return NULL  `pax_u` is NULL.
//! @return NULL  File could not be opened/created.
//! @return NULL  File size could not be determined.
//! @return       Binary file handle.
c3_bile*
c3_bile_open(const c3_path* const pax_u);

//! Append a raw byte array to a binary file.
//!
//! If a call to c3_bile_put_raw() fails, the effects of any partial writes will
//! be abandoned so that the binary file is left in a consistent state.
//!
//! @param[in] bil_u  Binary file handle.
//! @param[in] dat_v  Raw byte array.
//! @param[in] len_i  Length of `dat_v` in bytes.
//!
//! @return 0  `dat_v` is NULL.
//! @return 0  Failed to write data to binary file.
//! @return 1  Success.
c3_t
c3_bile_put_raw(c3_bile* const    bil_u,
                const void* const dat_v,
                const size_t      len_i);

//! Read a raw byte array from a binary file.
//!
//! The read occurs from the beginning of the file if c3_bile_get_raw() has not
//! been called on the binary file since c3_bile_open() was called *or* if the
//! last call to c3_bile_get_raw() reached EOF. The exact starting position of
//! the read can be determined by checking the `off_i` field of the c3_bile
//! handle. If a call to c3_bile_get_raw() fails, the read offset will be
//! restored to its previous location as if the call was never attempted.
//!
//! @param[in]  bil_u  Binary file handle.
//! @param[out] dat_v  Destination address to write raw byte array to. Must have
//!                    enough space for `len_i` bytes.
//! @param[in]  len_i  Length of `dat_v` in bytes.
//!
//! @return 0  `dat_v` is NULL.
//! @return 0  Failed to read data from binary file.
//! @return 1  Success.
c3_t
c3_bile_get_raw(c3_bile* const bil_u, void* const dat_v, const size_t len_i);

//! Gracefully dispose of a binary file's resources. Does not free the binary
//! file handle itself.
//!
//! @param[in] bil_u  Binary file handle.
void
c3_bile_close(c3_bile* const bil_u);

#endif /* ifndef C3_BILE_H */
