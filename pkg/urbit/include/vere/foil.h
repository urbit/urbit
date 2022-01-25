//! @file foil.h
//! Filesystem (async).

#ifndef U3_VERE_FOIL_H
#define U3_VERE_FOIL_H

#include <uv.h>

#include "c/types.h"

//! Directory entry.
typedef struct _u3_dent {
  c3_c*            nam_c;
  struct _u3_dent* nex_u;
} u3_dent;

//! Simple directory state.
typedef struct _u3_dire {
  c3_c*    pax_c;  //!< path of directory
  uv_file  fil_u;  //!< file, opened read-only to fsync
  u3_dent* all_u;  //!< file list
} u3_dire;

//! Load directory, blockingly. Create if nonexistent.
//!
//! @param[in] pax_c  Directory object or NULL.
u3_dire*
u3_foil_folder(const c3_c* pax_c);

#endif /* ifndef U3_VERE_FOIL_H */
