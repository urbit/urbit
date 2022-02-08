//! @file queue.h
//! FIFO/LIFO queue backed by a doubly linked list.

#ifndef C3_QUEUE_H
#define C3_QUEUE_H

#include <stddef.h>


//! Opaque queue type.
typedef struct _c3_queue c3_queue;

//! Opaque queue iterator type.
typedef struct _c3_queue_iter c3_queue_iter;

//! Iterator start point options.
enum {
  C3_QUEUE_ITER_FRONT,
  C3_QUEUE_ITER_BACK,
};


//! Create a new queue.
//!
//! @return NULL  Memory allocation failed.
//! @return       Heap-allocated pointer to the newly created queue. MUST be
//!               freed by caller when queue is ready for disposal.
c3_queue*
c3_queue_init(void);


//! Get number of nodes in `que_u`.
//!
//! @param[in] que_u
//!
//! @return 0  [que_u] is NULL.
//! @return    Number of nodes in the `que_u`.
size_t
c3_queue_length(const c3_queue* const que_u);


//! Get the last node's payload from the back of `que_u`.
//!
//! @param[in] que_u
//!
//! @return NULL  `que_u` is NULL.
//! @return NULL  `que_u` is empty.
//! @return       Heap-allocated pointer to the last node's payload in `que_u`.
//!               Must NOT be freed by caller.
void*
c3_queue_peek_back(const c3_queue* const que_u);

//! See c3_queue_peek_back().
void*
c3_queue_peek_front(const c3_queue* const que_u);


//! Remove the last node's payload from the back of `que_u`.
//!
//! @param[in] que_u
//!
//! @return NULL  `que_u` is NULL.
//! @return NULL  `que_u` is empty.
//! @return       Heap-allocated pointer to the payload of the node that was
//!               popped from the back of `que_u`. MUST be freed by caller.
void*
c3_queue_pop_back(c3_queue* const que_u);

//! See c3_queue_pop_back().
void*
c3_queue_pop_front(c3_queue* const que_u);


//! Push a new node onto the back of `que_u`.
//!
//! When pushing a new element onto `que_u`, `siz_i` bytes from `dat_v` are
//! copied to a heap-allocated block of memory managed internally by `que_u`.
//! `c3_queue_push_*()` return a pointer to this internally managed block of
//! memory. This pointer should NOT be freed by the caller. The memory should
//! instead be freed by the caller when an element is popped from `que_u`.
//!
//! @param[in] que_u
//! @param[in] dat_v  Payload.
//! @param[in] siz_i  Size of the payload in bytes.
//!
//! @return NULL  `que_u` is NULL.
//! @return NULL  `dat_v` is NULL.
//! @return       Heap-allocated pointer to the newly added node's payload.
//!               Must NOT be freed by caller.
void*
c3_queue_push_back(c3_queue* const que_u,
                   const void* const dat_v,
                   const size_t siz_i);

//! See c3_queue_push_back().
void*
c3_queue_push_front(c3_queue* const que_u,
                    const void* const dat_v,
                    const size_t siz_i);


//! Open an iterator over `que_u`.
//!
//! A queue can only have a single iterator open at any one time.
//!
//! @param[in] que_u
//! @param[in] sar_i  Side of queue to place the iterator at. Must be one of
//!                   C3_QUEUE_ITER_FRONT or C3_QUEUE_ITER_BACK.
//!
//! @return NULL  `que_u` is NULL.
//! @return NULL  `que_u` is empty.
//! @return NULL  `que_u` already has an open iterator.
//! @return NULL  `sar_i` is not one of C3_QUEUE_ITER_FRONT or
//!               C3_QUEUE_ITER_BACK.
//! @return       Heap-allocated pointer to the newly created iterator. Call
//!               c3_queue_iter_free() to dispose of an iterator.
c3_queue_iter*
c3_queue_iter_init(c3_queue* const que_u, const size_t sar_i);

//! Return the payload of the node currently pointed to by `itr_u` and advance
//! to the next node. Note that takes place front to back if C3_QUEUE_ITER_FRONT
//! was supplied to c3_queue_iter_init() and back to front otherwise (i.e.
//! C3_QUEUE_ITER_BACK) was supplied.
//!
//! @param que_u
//! @param itr_u
//!
//! @return NULL  `que_u` is NULL.
//! @return NULL  `que_u` is empty.
//! @return NULL  `itr_u` is NULL.
//! @return NULL  `itr_u` is not a valid iterator over `que_u`.
//! @return NULL  `itr_u` has advanced past the end of `que_u`.
//! @return       Heap-allocated pointer to the node under `itr_u`. Must NOT be
//!               freed by caller.
void*
c3_queue_iter_step(const c3_queue* const que_u, c3_queue_iter* const itr_u);

//! Close `itr_u` over `que_u`, freeing `itr_u`'s resources and marking `que_u`
//! as eligible for a new iterator, which can be created with
//! c3_queue_iter_init(). If `itr_u` does not belong to `que_u`, no action is
//! taken.
//!
//! @param[in] que_u  If NULL, no action is taken.
//! @param[in] itr_u  If NULL, no action is taken.
void
c3_queue_iter_free(c3_queue* const que_u, const c3_queue_iter* const itr_u);

#endif /* ifndef C3_QUEUE_H */
