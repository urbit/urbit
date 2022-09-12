/// @file path.h
///
/// Canonical-ish path. All references to empty components, `.`, and
/// `..` are removed, but symlinks are not resolved. The filesystem is not
/// consulted when reducing a path. As a result, a path like
/// `fanfun-mocbud/../../../` is reduced to an empty path because it resolves to
/// the grandparent directory of `fanfun-mocbud`, which cannot be known without
/// reading from the filesystem.

#ifndef C3_PATH_H
#define C3_PATH_H

#include <stdarg.h>
#include <stddef.h>
#include <unistd.h>

#include "c/portable.h"
#include "c/types.h"

//==============================================================================
// Types
//==============================================================================

/// A file path.
struct _c3_path;
typedef struct _c3_path c3_path;

//==============================================================================
// Functions
//==============================================================================

/// Construct a path from an array of strings.
///
/// @note mnemonic: `c3_path` [f]rom [a]rray -> `c3_path_fa`.
///
/// @param[in] tok_c  Array of path components. Must be valid C strings. If
///                   NULL, an empty path is created.
/// @param[in] tok_i  Number of elements in `toks_c`. If 0, an empty path is
///                   created.
///
/// @return  Path handle. Must be freed by caller.
c3_path*
c3_path_fa(const c3_c** toks_c, const size_t tok_i);

/// Construct a path from an existing path.
///
/// @note mnemonic: `c3_path` [f]rom [p]ath -> `c3_path_fp`.
///
/// @param[in] pax_u  Path handle. If NULL, an empty path is created.
///
/// @return  Path handle. Must be freed by caller.
c3_path*
c3_path_fp(const c3_path* const pax_u);

/// Construct a path from a variadic argument sequence.
///
/// @note mnemonic: `c3_path` [f]rom [v]ariadic -> `c3_path_fv`.
///
/// @param[in] tok_i  Number of components of the path. If 0, an empty path is
///                   created.
/// @param[in] ...    Components of the path. Must be valid C string(s).
///
/// @return  Path handle. Must be freed by caller.
c3_path*
c3_path_fv(const size_t tok_i, ...);

/// Get the string representation of a path.
///
/// @param[in] pax_u  Path handle.
///
/// @return  String representation of `pax_u` or NULL if `pax_u` is NULL or
///          empty.
const c3_c*
c3_path_str(const c3_path* const pax_u);

/// Push a component onto the end of a path.
///
/// Pushing `.`, the empty string, or NULL is a no-op. Pushing `..` is
/// equivalent to calling c3_path_pop() except in the case where the path is `/`
/// (i.e. `/..` is always `/`).
///
/// @param[in] pax_u  Path handle.
/// @param[in] tok_c  Path component to append to the path.
void
c3_path_push(c3_path* pax_u, const c3_c* const tok_c);

/// Pop a component from the end of a path.
///
/// @param[in] pax_u  Path handle.
void
c3_path_pop(c3_path* const pax_u);

/// Determine if two paths are equivalent.
///
/// @param[in] lef_u  Path handle.
/// @param[in] rih_u  Path handle.
///
/// @return 0  Paths don't match.
/// @return 1  Paths match.
c3_t
c3_path_eq(const c3_path* const lef_u, const c3_path* const rih_u);

/// Free a path.
///
/// @param[in] pax_u  Path handle.
void
c3_path_free(c3_path* const pax_u);

#endif /* ifndef C3_PATH_H */
