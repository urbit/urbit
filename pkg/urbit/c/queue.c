//! @file queue.c

#include "c/queue.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"


//! Single element of a doubly-linked list.
struct _node {
  void*         dat_v;  //!< payload data
  size_t        siz_i;  //!< size of payload data in bytes
  struct _node* nex_u;  //!< next node
  struct _node* pre_u;  //!< previous node
};
typedef struct _node _node;

//! Queue abstraction backed by a doubly-linked list.
struct _c3_queue {
  _node* fir_u;  //!< first node in queue
  _node* las_u;  //!< last node in queue
  size_t len_i;  //!< length of queue in nodes
};


c3_queue*
c3_queue_init(void)
{
  c3_queue* que_u = c3_calloc(sizeof(*que_u));
  return que_u;
}


size_t
c3_queue_length(const c3_queue* const que_u)
{
  return (NULL == que_u) ? 0 : que_u->len_i;
}
