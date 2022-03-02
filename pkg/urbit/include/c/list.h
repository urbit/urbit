//! @file list.h
//! Generic list abstraction backed by a doubly linked list.

#ifndef C3_LIST_H
#define C3_LIST_H

#include <stddef.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

//==============================================================================
// Types
//==============================================================================

//! Doubly-linked list node.
//!
//! The next, previous, and data fields should never be accessed
//! directly for compatibility reasons. Instead, use c3_list_next(),
//! c3_list_prev(), and c3_list_data() (respectively).
typedef struct _c3_list_node {
  struct _c3_list_node* nex_u;   //!< next node
  struct _c3_list_node* pre_u;   //!< previous node
  c3_y                  dat_y[]; //!< payload data
} c3_list_node;

//! Doubly-linked list handle.
//!
//! The length field should never be accessed directly for compatibility
//! reasons. Instead, use c3_list_len().
typedef struct {
  c3_list_node* fro_u; //!< node at front of list
  c3_list_node* bak_u; //!< node at back of list
  size_t        len_i; //!< number of nodes in list
} c3_list;

//! Sentinel values used to indicate which end of the list to operate on.
typedef enum {
  C3_LIST_FRONT = 0,
  C3_LIST_BACK  = 1,
} c3_list_end;

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
static inline size_t
c3_list_len(const c3_list* const lis_u)
{
  return lis_u ? lis_u->len_i : 0;
}

//! Get the successor node of a list node.
static inline c3_list_node*
c3_list_next(const c3_list_node* const nod_u)
{
  return nod_u->nex_u;
}

//! Get the predecessor node of a list node.
static inline c3_list_node*
c3_list_prev(const c3_list_node* const nod_u)
{
  return nod_u->pre_u;
}

//! Get the payload data from a list node.
static inline void*
c3_list_data(const c3_list_node* const nod_u)
{
  return (void*)nod_u->dat_y;
}

//! Add a new node onto the end of `lis_u`.
//!
//! When pushing a new element onto `lis_u`, `siz_i` bytes from `dat_v` are
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

//! Get an end node from `lis_u`.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_i  If C3_LIST_FRONT, peek the front of `lis_u`.
//!                   If C3_LIST_BACK, peek the back of `lis_u`.
//!                   If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
//!                   undefined.
//!
//! @return NULL  `lis_u` is empty.
//! @return       Specified end node of `lis_u`. MUST NOT be freed by caller.
c3_list_node*
c3_list_peek(const c3_list* const lis_u, const c3_list_end end_i);

//! Get back node from a list. See c3_list_peek().
static inline c3_list_node*
c3_list_peekb(const c3_list* const lis_u)
{
  return c3_list_peek(lis_u, C3_LIST_BACK);
}

//! Get front node from a list. See c3_list_peek().
static inline c3_list_node*
c3_list_peekf(const c3_list* const lis_u)
{
  return c3_list_peek(lis_u, C3_LIST_FRONT);
}

//! Remove an end node from `lis_u`.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_i  If C3_LIST_FRONT, pop the front of `lis_u`.
// If C3_LIST_BACK, pop the back of `lis_u`.
// If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
// undefined.
//
//! @return NULL  `lis_u` is empty.
//! @return       Specified end node of `lis_u`. MUST be freed by caller.
c3_list_node*
c3_list_pop(c3_list* const lis_u, const c3_list_end end_i);

//! Pop back node from a list. See c3_list_pop().
static inline c3_list_node*
c3_list_popb(c3_list* const lis_u)
{
  return c3_list_pop(lis_u, C3_LIST_BACK);
}

//! Pop front node from a list. See c3_list_pop().
static inline c3_list_node*
c3_list_popf(c3_list* const lis_u)
{
  return c3_list_pop(lis_u, C3_LIST_FRONT);
}

#endif /* ifndef C3_LIST_H */
