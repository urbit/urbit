//! @file queue.h
//! FIFO/LIFO queue backed by a doubly linked list.

#ifndef C3_QUEUE_H
#define C3_QUEUE_H

#include <stddef.h>

typedef struct _c3_queue c3_queue;

//! Create a new queue.
//!
//! @return NULL  Memory allocation failed.
//! @return       Heap-allocated pointer to the newly created queue.
c3_queue*
c3_queue_init(void);

//! Get number of nodes in the queue.
//!
//! @param[in] que_u  Queue to get the length of.
//!
//! @return 0  [que_u] was NULL.
//! @return    Number of nodes in the queue.
size_t
c3_queue_length(const c3_queue* que_u);

//! Add a node to the back of the queue.
//!
//! @param[in] que_u  Queue to append the node to.
//! @param[in] dat_v  Pointer to the node's payload.
//! @param[in] siz_i  Size of the node's payload in bytes.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the newly added node's payload.
void*
c3_queue_push_back(c3_queue* que_u, void* dat_v, size_t siz_i);

//! Add a node to the front of the queue.
//!
//! @param[in] que_u  Queue to prepend the node to.
//! @param[in] dat_v  Pointer to the node's payload.
//! @param[in] siz_i  Size of the node's payload in bytes.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the newly added node's payload.
void*
c3_queue_push_front(c3_queue* que_u, void* dat_v, size_t siz_i);

//! Get the last node's data from the back of the queue.
//!
//! @param[in] que_u  Queue to get the last node's data from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the last node's payload.
void*
c3_queue_peek_back(const c3_queue* que_u);

//! Get the first node's data from the front of the queue.
//!
//! @param[in] que_u  Queue to get the first node's data from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the first node's payload.
void*
c3_queue_peek_front(const c3_queue* que_u);

//! Remove the last node from the back of the queue.
//!
//! @param[in] que_u  Queue to remove the last node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to node's payload popped from the back
//!               of [que_u].
void*
c3_queue_pop_back(c3_queue* que_u);

//! Remove the first node from the front of the queue.
//!
//! @param[in] que_u  Queue to remove the first node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to node's payload popped from the front
//!               of [que_u].
void*
c3_queue_pop_front(c3_queue* que_u);

//! Free the queue and all of its nodes.
//!
//! @param que_u  Queue to free. If NULL, no operation occurs.
void
c3_queue_free(c3_queue* que_u);

#endif /* ifndef C3_QUEUE_H */
