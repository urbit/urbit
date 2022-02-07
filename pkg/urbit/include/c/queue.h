//! @file queue.h
//! FIFO/LIFO queue backed by a doubly linked list.

#ifndef C3_QUEUE_H
#define C3_QUEUE_H

#include <stddef.h>


//! Opaque queue type.
typedef struct _c3_queue c3_queue;


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
//! @return 0  [que_u] was NULL.
//! @return    Number of nodes in the `que_u`.
size_t
c3_queue_length(const c3_queue* const que_u);


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
//! @return NULL  `que_u` was NULL.
//! @return NULL  `dat_v` was NULL.
//! @return       Heap-allocated pointer to the newly added node's payload.
//!               Should NOT be freed by caller.
void*
c3_queue_push_back(c3_queue* const que_u,
                   const void* const dat_v,
                   const size_t siz_i);

//! See c3_queue_push_back().
void*
c3_queue_push_front(c3_queue* const que_u,
                    const void* const dat_v,
                    const size_t siz_i);


//! Get the last node from the back of `que_u`.
//!
//! @param[in] que_u
//!
//! @return NULL  `que_u` was NULL.
//! @return NULL  `que_u` was empty.
//! @return       Heap-allocated pointer to the last node's payload in `que_u`.
//!               Should NOT be freed by caller.
void*
c3_queue_peek_back(const c3_queue* const que_u);

//! See c3_queue_peek_back().
void*
c3_queue_peek_front(const c3_queue* const que_u);


//! Remove the last node from the back of `que_u`.
//!
//! @param[in] que_u
//!
//! @return NULL  `que_u` was NULL.
//! @return       Heap-allocated pointer to the payload of the node that was
//!               popped from the back of `que_u`. MUST be freed by caller.
void*
c3_queue_pop_back(c3_queue* const que_u);

//! See c3_queue_pop_back().
void*
c3_queue_pop_front(c3_queue* const que_u);

#endif /* ifndef C3_QUEUE_H */
