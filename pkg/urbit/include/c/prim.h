/// @file
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
// Macros
//==============================================================================

#define declare_put_num(type, type_suffix)                                     \
  /** Write an integer of type <type> to a backing file.                       \
   *                                                                           \
   *  If the backing file already exists, its contents will be overwritten.    \
   *                                                                           \
   *  @param[in] path_u             Path to backing file.                      \
   *  @param[in] num_<type_suffix>  Integer.                                   \
   *                                                                           \
   * @return 1  Integer successfully written to backing file.                  \
   * @return 0  Otherwise.                                                     \
   */                                                                          \
  c3_t c3_prim_put_##type(const c3_path* const path_u,                         \
                          c3_##type_suffix     num_##type_suffix)

#define declare_get_num(type, type_suffix)                                     \
  /** Read a persistent integer from its backing file.                         \
   *                                                                           \
   *  @param[in] path_u              Path to backing file.                     \
   *  @param[out] num_<type_suffix>  Output pointer for persistent integer.    \
   *                                                                           \
   *  @return 1  Integer successfully read from backing file.                  \
   *  @return 0  Otherwise.                                                    \
   */                                                                          \
  c3_t c3_prim_get_##type(const c3_path* const    path_u,                      \
                          c3_##type_suffix* const num_##type_suffix)

//==============================================================================
// Functions
//==============================================================================

/// Write a string to a backing file.
///
/// @param[in] path_u  Path to backing file.
/// @param[in] str_c   String.
///
/// @return 1  String successfully written to backing file.
/// @return 0  Otherwise.
c3_t
c3_prim_put_str(const c3_path* const path_u, const c3_c* const str_c);

declare_put_num(uint8, y);
declare_put_num(uint16, s);
declare_put_num(uint32, w);
declare_put_num(uint64, d);

declare_put_num(int8, ys);
declare_put_num(int16, ss);
declare_put_num(int32, ws);
declare_put_num(int64, ds);

/// Read a persistent string from its backing file.
///
/// @param[in]  path_u  Path to backing file.
/// @param[out] str_c   Output pointer for the persistent string.
///
/// @return 1  String successfully read from backing file.
/// @return 0  Otherwise.
c3_t
c3_prim_get_str(const c3_path* const path_u, c3_c** str_c);

declare_get_num(uint8, y);
declare_get_num(uint16, s);
declare_get_num(uint32, w);
declare_get_num(uint64, d);

declare_get_num(int8, ys);
declare_get_num(int16, ss);
declare_get_num(int32, ws);
declare_get_num(int64, ds);

#undef declare_put_num
#undef declare_get_num

#endif /* ifndef C3_PRIM_H */
