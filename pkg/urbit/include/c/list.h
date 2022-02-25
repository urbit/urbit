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

//!< Doubly-linked list node.
typedef struct _c3_list_node {
  struct _c3_list_node* nex_u;    //!< next node
  struct _c3_list_node* pre_u;    //!< previous node
  c3_y                  dat_y[];  //!< payload data
} c3_list_node;

//!< Doubly-linked list handle.
typedef struct {
  c3_list_node* fro_u;  //!< node at front of list
  c3_list_node* bak_u;  //!< node at back of list
  size_t        len_i;  //!< number of nodes in list
} c3_list;

//! Sentinel values used to indicate the end of the list to operate on.
typedef enum {
  C3_LIST_FRONT = 0,
  C3_LIST_BACK  = 1,
} c3_list_end;

//==============================================================================
// Macros
//==============================================================================

//! Number of nodes in `lis`.
#define c3_list_len(lis)                                                       \
  ((lis) ? (lis)->len_i : 0)

//! Get back node from `lis`.
#define c3_list_peekb(lis)                                                     \
  c3_list_peek(lis, C3_LIST_BACK)

//! Get front node from `lis`.
#define c3_list_peekf(lis)                                                     \
  c3_list_peek(lis, C3_LIST_FRONT)

//! Push a new node onto the back of `lis`.
#define c3_list_pushb(lis, dat, siz)                                           \
  c3_list_push(lis, C3_LIST_BACK, dat, siz)

//! Push a new node onto the front of `lis`.
#define c3_list_pushf(lis, dat, siz)                                           \
  c3_list_push(lis, C3_LIST_FRONT, dat, siz)

//! Pop back node from `lis`.
#define c3_list_popb(lis)                                                      \
  c3_list_pop(lis, C3_LIST_BACK)

//! Pop front node from `lis`.
#define c3_list_popf(lis)                                                      \
  c3_list_pop(lis, C3_LIST_FRONT)

//! Get the successor node of `nod`. NULL if `nod` is the back of the list.
#define c3_list_next(nod)                                                      \
  ((nod) ? (nod)->nex_u : NULL)

//! Get the predecessor node of `nod`. NULL if `nod` is the front of the list.
#define c3_list_prev(nod)                                                      \
  ((nod) ? (nod)->pre_u : NULL)

//! Extract the payload data from `nod` as a raw pointer. MUST NOT be freed.
#define c3_list_data(nod)                                                      \
  ((void*)((nod)->dat_y))

//==============================================================================
// Functions
//==============================================================================

//! Create a new list.
//!
//! @return  Newly created list. MUST be freed by caller when list is ready for
//!          disposal.
c3_list*
c3_list_init(void);

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
c3_list_peek(const c3_list* const lis_u,
             const c3_list_end    end_i);

//! Remove an end node from `lis_u`.
//!
//! @param[in] lis_u  If NULL, behavior is undefined.
//! @param[in] end_i  If C3_LIST_FRONT, pop the front of `lis_u`.
//                    If C3_LIST_BACK, pop the back of `lis_u`.
//                    If neither C3_LIST_FRONT nor C3_LIST_BACK, behavior is
//                    undefined.
//
//! @return NULL  `lis_u` is empty.
//! @return       Specified end node of `lis_u`. MUST be freed by caller.
c3_list_node*
c3_list_pop(c3_list* const    lis_u,
            const c3_list_end end_i);

#endif /* ifndef C3_LIST_H */
