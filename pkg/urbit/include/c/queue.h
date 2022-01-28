//! @file queue.h
//! FIFO/LIFO queue backed by a doubly linked list.

#ifndef C3_QUEUE_H
#define C3_QUEUE_H

#include <stddef.h>

//! Single element of a doubly-linked list.
typedef struct _c3_node {
  void*            dat_v;  //!< data
  size_t           siz_i;  //!< size of data in bytes
  struct _c3_node* nex_u;  //!< next node
  struct _c3_node* pre_u;  //!< previous node
} c3_node;

//! Queue abstraction backed by a doubly-linked list.
typedef struct _c3_queue {
  c3_node* fir_u;  //!< first node in queue
  c3_node* las_u;  //!< last node in queue
  size_t   len_i;  //!< length of queue in nodes
} c3_queue;

//! Create a new queue.
//!
//! @return NULL  memory allocation failed.
//! @return       heap-allocated pointer to the newly created queue.
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
//! @return       Heap-allocated pointer to the newly added node.
c3_node*
c3_queue_push_back(c3_queue* que_u, void* dat_v, size_t siz_i);

//! Add a node to the front of the queue.
//!
//! @param[in] que_u  Queue to prepend the node to.
//! @param[in] dat_v  Pointer to the node's payload.
//! @param[in] siz_i  Size of the node's payload in bytes.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the newly added node.
c3_node*
c3_queue_push_front(c3_queue* que_u, void* dat_v, size_t siz_i);

//! Get the last node from the back of the queue.
//!
//! @param[in] que_u  Queue to get the last node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the last node in [que_u].
c3_node*
c3_queue_peek_back(const c3_queue* que_u);

//! Get the first node from the front of the queue.
//!
//! @param[in] que_u  Queue to get the first node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to the first node in [que_u].
c3_node*
c3_queue_peek_front(const c3_queue* que_u);

//! Remove the last node from the back of the queue.
//!
//! @param[in] que_u  Queue to remove the last node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to node popped from the back of
//!               [que_u].
c3_node*
c3_queue_pop_back(c3_queue* que_u);

//! Remove the first node from the front of the queue.
//!
//! @param[in] que_u  Queue to remove the first node from.
//!
//! @return NULL  [que_u] was NULL.
//! @return       Heap-allocated pointer to node popped from the front of
//!               [que_u].
c3_node*
c3_queue_pop_front(c3_queue* que_u);

//! Free the queue and all of its nodes.
//!
//! @param que_u  Queue to free. If NULL, no operation occurs.
void
c3_queue_free(c3_queue* que_u);

#endif /* ifndef C3_QUEUE_H */
