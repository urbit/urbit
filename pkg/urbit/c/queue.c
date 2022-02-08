//! @file queue.c

#include "c/queue.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

//==============================================================================
// Types
//==============================================================================

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
  _node*         fir_u;  //!< first node in queue
  _node*         las_u;  //!< last node in queue
  size_t         len_i;  //!< length of queue in nodes
  c3_queue_iter* itr_u;  //!< NULL if no open iterator
};

//! Queue iterator.
struct _c3_queue_iter {
  size_t sar_i;  //!< starting location
  _node* cur_u;  //!< current node in queue
};

//==============================================================================
// Static functions
//==============================================================================

static _node*
_create_node(const void* const dat_v, size_t siz_i)
{
  _node* nod_u = c3_calloc(sizeof(*nod_u));
  nod_u->siz_i = siz_i;
  nod_u->dat_v = c3_malloc(nod_u->siz_i);
  memcpy(nod_u->dat_v, dat_v, nod_u->siz_i);
  return nod_u;
}

//==============================================================================
// Functions
//==============================================================================

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

void*
c3_queue_peek_back(const c3_queue* const que_u)
{
  return (0 == c3_queue_length(que_u)) ? NULL : que_u->las_u->dat_v;
}

void*
c3_queue_peek_front(const c3_queue* const que_u)
{
  return (0 == c3_queue_length(que_u)) ? NULL : que_u->fir_u->dat_v;
}

void*
c3_queue_pop_back(c3_queue* const que_u)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  if ( NULL == que_u->las_u ) {
    c3_assert(NULL == que_u->fir_u);
    return NULL;
  }

  _node* nod_u = que_u->las_u;
  que_u->las_u = que_u->las_u->pre_u;
  if ( NULL == que_u->las_u ) {
    que_u->fir_u = NULL;
  }
  else {
    que_u->las_u->nex_u = NULL;
  }
  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);
  que_u->len_i--;

  return dat_v;
}

void*
c3_queue_pop_front(c3_queue* const que_u)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  if ( NULL == que_u->fir_u ) {
    c3_assert(NULL == que_u->las_u);
    return NULL;
  }

  _node* nod_u = que_u->fir_u;
  que_u->fir_u = que_u->fir_u->nex_u;
  if ( NULL == que_u->fir_u ) {
    que_u->las_u = NULL;
  }
  else {
    que_u->fir_u->pre_u = NULL;
  }
  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);
  que_u->len_i--;

  return dat_v;
}
void*
c3_queue_push_back(c3_queue* const que_u,
                   const void* const dat_v,
                   const size_t siz_i)
{
  if ( NULL == que_u || NULL == dat_v ) {
    return NULL;
  }

  _node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_queue_length(que_u) ) {
    c3_assert(NULL == que_u->fir_u && NULL == que_u->las_u);
    que_u->fir_u = nod_u;
    que_u->las_u = nod_u;
  }
  else {
    c3_assert(NULL != que_u->fir_u && NULL != que_u->las_u);
    que_u->las_u->nex_u = nod_u;
    nod_u->pre_u = que_u->las_u;
    que_u->las_u = nod_u;
  }
  que_u->len_i++;

  return nod_u->dat_v;
}

void*
c3_queue_push_front(c3_queue* const que_u,
                    const void* const dat_v,
                    const size_t siz_i)
{
  if ( NULL == que_u || NULL == dat_v ) {
    return NULL;
  }

  _node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_queue_length(que_u) ) {
    c3_assert(NULL == que_u->fir_u && NULL == que_u->las_u);
    que_u->fir_u = nod_u;
    que_u->las_u = nod_u;
  }
  else {
    c3_assert(NULL != que_u->fir_u && NULL != que_u->las_u);
    que_u->fir_u->pre_u = nod_u;
    nod_u->nex_u = que_u->fir_u;
    que_u->fir_u = nod_u;
  }
  que_u->len_i++;

  return nod_u->dat_v;
}

c3_queue_iter*
c3_queue_iter_init(c3_queue* const que_u, const size_t sar_i)
{
  if ( 0 == c3_queue_length(que_u) || NULL != que_u->itr_u ) {
    return NULL;
  }

  que_u->itr_u = c3_malloc(sizeof(que_u->itr_u));
  que_u->itr_u->sar_i = sar_i;

  switch ( que_u->itr_u->sar_i ) {
    case C3_QUEUE_ITER_FRONT:
      que_u->itr_u->cur_u = que_u->fir_u;
      break;
    case C3_QUEUE_ITER_BACK:
      que_u->itr_u->cur_u = que_u->las_u;
      break;
    default:
      c3_free(que_u->itr_u);
      que_u->itr_u = NULL;
      break;
  }
  return que_u->itr_u;
}

void*
c3_queue_iter_step(const c3_queue* const que_u, c3_queue_iter* const itr_u)
{
  if ( 0 == c3_queue_length(que_u) ||
       NULL == itr_u               ||
       que_u->itr_u != itr_u       ||
       NULL == itr_u->cur_u )
  {
    return NULL;
  }

  void* dat_v = itr_u->cur_u->dat_v;
  switch ( itr_u->sar_i ) {
    case C3_QUEUE_ITER_FRONT:
      itr_u->cur_u = itr_u->cur_u->nex_u;
      break;
    case C3_QUEUE_ITER_BACK:
      itr_u->cur_u = itr_u->cur_u->pre_u;
      break;
    default:
      c3_assert(!"queue: invalid starting location");
  }
  return dat_v;
}

void
c3_queue_iter_free(c3_queue* const que_u, const c3_queue_iter* const itr_u) {
  if ( NULL == que_u || NULL == itr_u || que_u->itr_u != itr_u ) {
    return;
  }
  c3_free(itr_u);
  que_u->itr_u = NULL;
}
