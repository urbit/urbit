/// @file prim.h
///
/// A file-backed, persistent primitive variable.
///
/// @note mnemonic: [prim]itive variable -> prim

#ifndef C3_PRIM_H
#define C3_PRIM_H

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"
#include "c/path.h"

//==============================================================================
// Types
//==============================================================================

/// The supported primitive types.
typedef enum {
  /// A NULL-terminated sequence of characters.
  c3_prim_str,

  /// An unsigned 8-bit integer.
  c3_prim_uint8,

  /// An unsigned 16-bit integer.
  c3_prim_uint16,

  /// An unsigned 32-bit integer.
  c3_prim_uint32,

  /// An unsigned 64-bit integer.
  c3_prim_uint64,

  /// A signed 8-bit integer.
  c3_prim_int8,

  /// A signed 16-bit integer.
  c3_prim_int16,

  /// A signed 32-bit integer.
  c3_prim_int32,

  /// A signed 64-bit integer.
  c3_prim_int64,

  // A double precision floating point number.
  // @warning watch out for rounding bugs.
  c3_prim_float,

  /// Sentinel value. Not actually a type.
  c3_prim_last,

} c3_prim_type;

//==============================================================================
// Functions
//==============================================================================

/// Read a persistant variable from its backing file.
///
/// @param[in]  path_u      Path to backing file.
/// @param[in]  type_e      Type of variable.
/// @param[out] data_v      Pointer to the variable.
///
/// @note The type of `data_v` should have one additional layer of indirection
///       from the type indicated in `type_e`. So if `type_e` is `c3_prim_str`,
///       `data_v` should be `c3_c**`, and if `type_e` is `c3_prim_uint32`,
///       `data_v` should be `c3_w*`.
///
/// @return 1  Variable successfully read from backing file.
/// @return 0  Otherwise.
c3_t
c3_prim_get(const c3_path* const path_u,
            const c3_prim_type   type_e,
            void*                data_v);

/// Write a variable to a backing file.
///
/// If the backing file already exists, its contents will be overwritten.
///
/// @param[in] path_u      Path to backing file.
/// @param[in] type_e      Type of variable.
/// @param[in] data_v      Pointer to the variable.
///
/// @note Just as with c3_prim_get(), the type of `data_v` should have one
///       additional layer of indirection from the type indicated in `type_e`.
///
/// @return 1  Variable successfully written to backing file.
/// @return 0  Otherwise.
c3_t
c3_prim_put(const c3_path* const path_u,
            const c3_prim_type   type_e,
            const void* const    data_v);

#endif /* ifndef C3_PRIM_H */
