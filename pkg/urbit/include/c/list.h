//! @file list.h
//!
//! Generic list abstraction with O(1) push/pop.

#ifndef C3_LIST_H
#define C3_LIST_H

#include <stddef.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

//==============================================================================
// Types
//==============================================================================

//! List.
struct _c3_list;
typedef struct _c3_list c3_list;

//! List node.
struct _c3_list_node;
typedef struct _c3_list_node c3_lode;

//! Sentinel values used to indicate which end of the list to operate on.
typedef enum {
  C3_LIST_FRONT = 0,
  C3_LIST_BACK  = 1,
} c3_list_end;

//==============================================================================
// Macros
//==============================================================================

//! Error handling wrapper for c3_list API calls that return 0/NULL on failure
//! and non-zero/non-NULL on success.
//!
//! @param[in] list_call       c3_list API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_list(list_call, failure_action, ...)                               \
  do {                                                                         \
    if ( !(list_call) ) {                                                      \
      fprintf(stderr, "list: " __VA_ARGS__);                                   \
      fprintf(stderr, "\r\n");                                                 \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Functions
//==============================================================================

//! Create a new list.
//!
//! @return  Newly created list. MUST be freed by caller when list is ready for
//!          disposal.
c3_list*
c3_list_init(void);

//! Get number of nodes in a list.
//!
//! @param[in] lis_u  List handle.
//!
//! @return  Number of nodes in `lis_u` or 0 if `lis_u` is NULL.
size_t
c3_list_len(const c3_list* const lis_u);

//! Add a new node onto the end of a list.
//!
//! When pushing a new element onto a list, `siz_i` bytes from `dat_v` are
//! copied to the payload field of the newly allocated node.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_u  If C3_LIST_FRONT, push onto the front of `lis_u`.
//!                   If C3_LIST_BACK, push onto the back of `lis_u`.
//!                   If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
//!                   undefined.
//! @param[in] dat_v  Payload. If NULL, behavior is undefined.
//! @param[in] siz_i  Size of the payload in bytes.
void
c3_list_push(c3_list* const    lis_u,
             const c3_list_end end_i,
             const void* const dat_v,
             const size_t      siz_i);

//! Push a new node onto the back of a list. See c3_list_push().
static inline void
c3_list_pushb(c3_list* const lis_u, const void* const dat_v, const size_t siz_i)
{
  c3_list_push(lis_u, C3_LIST_BACK, dat_v, siz_i);
}

//! Push a new node onto the front of a list. See c3_list_push().
static inline void
c3_list_pushf(c3_list* const lis_u, const void* const dat_v, const size_t siz_i)
{
  c3_list_push(lis_u, C3_LIST_FRONT, dat_v, siz_i);
}

//! Get an end node from a list.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_i  If C3_LIST_FRONT, peek the front of `lis_u`.
//!                   If C3_LIST_BACK, peek the back of `lis_u`.
//!                   If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
//!                   undefined.
//!
//! @return NULL  `lis_u` is empty.
//! @return       Specified end node of `lis_u`. Must NOT be freed by caller.
c3_lode*
c3_list_peek(const c3_list* const lis_u, const c3_list_end end_i);

//! Get back node from a list. See c3_list_peek().
static inline c3_lode*
c3_list_peekb(const c3_list* const lis_u)
{
  return c3_list_peek(lis_u, C3_LIST_BACK);
}

//! Get front node from a list. See c3_list_peek().
static inline c3_lode*
c3_list_peekf(const c3_list* const lis_u)
{
  return c3_list_peek(lis_u, C3_LIST_FRONT);
}

//! Remove an end node from a list.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_i  If C3_LIST_FRONT, pop the front of `lis_u`.
//!                   If C3_LIST_BACK, pop the back of `lis_u`.
//!                   If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
//!                   undefined.
//!
//! @return NULL  `lis_u` is empty.
//! @return       Specified end node of `lis_u`. Must be freed by caller.
c3_lode*
c3_list_pop(c3_list* const lis_u, const c3_list_end end_i);

//! Pop back node from a list. See c3_list_pop().
static inline c3_lode*
c3_list_popb(c3_list* const lis_u)
{
  return c3_list_pop(lis_u, C3_LIST_BACK);
}

//! Pop front node from a list. See c3_list_pop().
static inline c3_lode*
c3_list_popf(c3_list* const lis_u)
{
  return c3_list_pop(lis_u, C3_LIST_FRONT);
}

//! Get the successor of a list node.
//!
//! @param[in]  List node handle.
//!
//! @return  Successor of `nod_u` or NULL if `nod_u` is NULL.
c3_lode*
c3_lode_next(const c3_lode* const nod_u);

//! Get the predecessor of a list node.
//!
//! @param[in]  List node handle.
//!
//! @return  Predecessor of `nod_u` or NULL if `nod_u` is NULL.
c3_lode*
c3_lode_prev(const c3_lode* const nod_u);

//! Get the payload data of a list node.
//!
//! @param[in] nod_u  List node handle.
//!
//! @return  Payload data of `nod_u` or NULL if `nod_u` is NULL.
void*
c3_lode_data(const c3_lode* const nod_u);

//! Get the length in bytes of a list node's payload data.
//!
//! @param[in] nod_u  List node handle.
//!
//! @return  Length of payload data of `nod_u` or 0 if `nod_u` is NULL.
size_t
c3_lode_len(const c3_lode* const nod_u);

#endif /* ifndef C3_LIST_H */
