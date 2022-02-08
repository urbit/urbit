//! @file list.h
//! Generic list abstraction backed by a doubly linked list.

#ifndef C3_LIST_H
#define C3_LIST_H

#include <stddef.h>


//! Opaque list type.
typedef struct _c3_list c3_list;

//! Opaque list iterator type.
typedef struct _c3_list_iter c3_list_iter;

//! Iterator start point options.
enum {
  C3_LIST_ITER_FRONT,
  C3_LIST_ITER_BACK,
};


//! Create a new list.
//!
//! @return NULL  Memory allocation failed.
//! @return       Heap-allocated pointer to the newly created list. MUST be
//!               freed by caller when list is ready for disposal.
c3_list*
c3_list_init(void);


//! Get number of nodes in `lis_u`.
//!
//! @param[in] lis_u
//!
//! @return 0  [lis_u] is NULL.
//! @return    Number of nodes in the `lis_u`.
size_t
c3_list_length(const c3_list* const lis_u);


//! Get the last node's payload from the back of `lis_u`.
//!
//! @param[in] lis_u
//!
//! @return NULL  `lis_u` is NULL.
//! @return NULL  `lis_u` is empty.
//! @return       Heap-allocated pointer to the last node's payload in `lis_u`.
//!               Must NOT be freed by caller.
void*
c3_list_peek_back(const c3_list* const lis_u);

//! See c3_list_peek_back().
void*
c3_list_peek_front(const c3_list* const lis_u);


//! Remove the last node's payload from the back of `lis_u`.
//!
//! @param[in] lis_u
//!
//! @return NULL  `lis_u` is NULL.
//! @return NULL  `lis_u` is empty.
//! @return       Heap-allocated pointer to the payload of the node that was
//!               popped from the back of `lis_u`. MUST be freed by caller.
void*
c3_list_pop_back(c3_list* const lis_u);

//! See c3_list_pop_back().
void*
c3_list_pop_front(c3_list* const lis_u);


//! Push a new node onto the back of `lis_u`.
//!
//! When pushing a new element onto `lis_u`, `siz_i` bytes from `dat_v` are
//! copied to a heap-allocated block of memory managed internally by `lis_u`.
//! `c3_list_push_*()` return a pointer to this internally managed block of
//! memory. This pointer should NOT be freed by the caller. The memory should
//! instead be freed by the caller when an element is popped from `lis_u`.
//!
//! @param[in] lis_u
//! @param[in] dat_v  Payload.
//! @param[in] siz_i  Size of the payload in bytes.
//!
//! @return NULL  `lis_u` is NULL.
//! @return NULL  `dat_v` is NULL.
//! @return       Heap-allocated pointer to the newly added node's payload.
//!               Must NOT be freed by caller.
void*
c3_list_push_back(c3_list* const lis_u,
                   const void* const dat_v,
                   const size_t siz_i);

//! See c3_list_push_back().
void*
c3_list_push_front(c3_list* const lis_u,
                    const void* const dat_v,
                    const size_t siz_i);


//! Open an iterator over `lis_u`.
//!
//! A list can only have a single iterator open at any one time.
//!
//! @param[in] lis_u
//! @param[in] sar_i  Side of list to place the iterator at. Must be one of
//!                   C3_LIST_ITER_FRONT or C3_LIST_ITER_BACK.
//!
//! @return NULL  `lis_u` is NULL.
//! @return NULL  `lis_u` is empty.
//! @return NULL  `lis_u` already has an open iterator.
//! @return NULL  `sar_i` is not one of C3_LIST_ITER_FRONT or
//!               C3_LIST_ITER_BACK.
//! @return       Heap-allocated pointer to the newly created iterator. Call
//!               c3_list_iter_free() to dispose of an iterator.
c3_list_iter*
c3_list_iter_init(c3_list* const lis_u, const size_t sar_i);

//! Return the payload of the node currently pointed to by `itr_u` and advance
//! to the next node. Note that takes place front to back if C3_LIST_ITER_FRONT
//! was supplied to c3_list_iter_init() and back to front otherwise (i.e.
//! C3_LIST_ITER_BACK) was supplied.
//!
//! @param lis_u
//! @param itr_u
//!
//! @return NULL  `lis_u` is NULL.
//! @return NULL  `lis_u` is empty.
//! @return NULL  `itr_u` is NULL.
//! @return NULL  `itr_u` is not a valid iterator over `lis_u`.
//! @return NULL  `itr_u` has advanced past the end of `lis_u`.
//! @return       Heap-allocated pointer to the node under `itr_u`. Must NOT be
//!               freed by caller.
void*
c3_list_iter_step(const c3_list* const lis_u, c3_list_iter* const itr_u);

//! Close `itr_u` over `lis_u`, freeing `itr_u`'s resources and marking `lis_u`
//! as eligible for a new iterator, which can be created with
//! c3_list_iter_init(). If `itr_u` does not belong to `lis_u`, no action is
//! taken.
//!
//! @param[in] lis_u  If NULL, no action is taken.
//! @param[in] itr_u  If NULL, no action is taken.
void
c3_list_iter_free(c3_list* const lis_u, c3_list_iter* const itr_u);

#endif /* ifndef C3_LIST_H */
