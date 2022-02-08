//! @file list.c

#include "c/list.h"

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
struct _c3_list {
  _node*         fir_u;  //!< first node in list
  _node*         las_u;  //!< last node in list
  size_t         len_i;  //!< length of list in nodes
  c3_list_iter* itr_u;  //!< NULL if no open iterator
};

//! Queue iterator.
struct _c3_list_iter {
  size_t sar_i;  //!< starting location
  _node* cur_u;  //!< current node in list
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

c3_list*
c3_list_init(void)
{
  c3_list* lis_u = c3_calloc(sizeof(*lis_u));
  return lis_u;
}

size_t
c3_list_length(const c3_list* const lis_u)
{
  return (NULL == lis_u) ? 0 : lis_u->len_i;
}

void*
c3_list_peek_back(const c3_list* const lis_u)
{
  return (0 == c3_list_length(lis_u)) ? NULL : lis_u->las_u->dat_v;
}

void*
c3_list_peek_front(const c3_list* const lis_u)
{
  return (0 == c3_list_length(lis_u)) ? NULL : lis_u->fir_u->dat_v;
}

void*
c3_list_pop_back(c3_list* const lis_u)
{
  if ( NULL == lis_u ) {
    return NULL;
  }

  if ( NULL == lis_u->las_u ) {
    c3_assert(NULL == lis_u->fir_u);
    return NULL;
  }

  _node* nod_u = lis_u->las_u;
  lis_u->las_u = lis_u->las_u->pre_u;
  if ( NULL == lis_u->las_u ) {
    lis_u->fir_u = NULL;
  }
  else {
    lis_u->las_u->nex_u = NULL;
  }
  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);
  lis_u->len_i--;

  return dat_v;
}

void*
c3_list_pop_front(c3_list* const lis_u)
{
  if ( NULL == lis_u ) {
    return NULL;
  }

  if ( NULL == lis_u->fir_u ) {
    c3_assert(NULL == lis_u->las_u);
    return NULL;
  }

  _node* nod_u = lis_u->fir_u;
  lis_u->fir_u = lis_u->fir_u->nex_u;
  if ( NULL == lis_u->fir_u ) {
    lis_u->las_u = NULL;
  }
  else {
    lis_u->fir_u->pre_u = NULL;
  }
  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);
  lis_u->len_i--;

  return dat_v;
}
void*
c3_list_push_back(c3_list* const lis_u,
                   const void* const dat_v,
                   const size_t siz_i)
{
  if ( NULL == lis_u || NULL == dat_v ) {
    return NULL;
  }

  _node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_list_length(lis_u) ) {
    c3_assert(NULL == lis_u->fir_u && NULL == lis_u->las_u);
    lis_u->fir_u = nod_u;
    lis_u->las_u = nod_u;
  }
  else {
    c3_assert(NULL != lis_u->fir_u && NULL != lis_u->las_u);
    lis_u->las_u->nex_u = nod_u;
    nod_u->pre_u = lis_u->las_u;
    lis_u->las_u = nod_u;
  }
  lis_u->len_i++;

  return nod_u->dat_v;
}

void*
c3_list_push_front(c3_list* const lis_u,
                    const void* const dat_v,
                    const size_t siz_i)
{
  if ( NULL == lis_u || NULL == dat_v ) {
    return NULL;
  }

  _node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_list_length(lis_u) ) {
    c3_assert(NULL == lis_u->fir_u && NULL == lis_u->las_u);
    lis_u->fir_u = nod_u;
    lis_u->las_u = nod_u;
  }
  else {
    c3_assert(NULL != lis_u->fir_u && NULL != lis_u->las_u);
    lis_u->fir_u->pre_u = nod_u;
    nod_u->nex_u = lis_u->fir_u;
    lis_u->fir_u = nod_u;
  }
  lis_u->len_i++;

  return nod_u->dat_v;
}

c3_list_iter*
c3_list_iter_init(c3_list* const lis_u, const size_t sar_i)
{
  if ( 0 == c3_list_length(lis_u) || NULL != lis_u->itr_u ) {
    return NULL;
  }

  lis_u->itr_u = c3_malloc(sizeof(lis_u->itr_u));
  lis_u->itr_u->sar_i = sar_i;

  switch ( lis_u->itr_u->sar_i ) {
    case C3_LIST_ITER_FRONT:
      lis_u->itr_u->cur_u = lis_u->fir_u;
      break;
    case C3_LIST_ITER_BACK:
      lis_u->itr_u->cur_u = lis_u->las_u;
      break;
    default:
      c3_free(lis_u->itr_u);
      lis_u->itr_u = NULL;
      break;
  }
  return lis_u->itr_u;
}

void*
c3_list_iter_step(const c3_list* const lis_u, c3_list_iter* const itr_u)
{
  if ( 0 == c3_list_length(lis_u) ||
       NULL == itr_u               ||
       lis_u->itr_u != itr_u       ||
       NULL == itr_u->cur_u )
  {
    return NULL;
  }

  void* dat_v = itr_u->cur_u->dat_v;
  switch ( itr_u->sar_i ) {
    case C3_LIST_ITER_FRONT:
      itr_u->cur_u = itr_u->cur_u->nex_u;
      break;
    case C3_LIST_ITER_BACK:
      itr_u->cur_u = itr_u->cur_u->pre_u;
      break;
    default:
      c3_assert(!"list: invalid starting location");
  }
  return dat_v;
}

void
c3_list_iter_free(c3_list* const lis_u, c3_list_iter* const itr_u) {
  if ( NULL == lis_u || NULL == itr_u || lis_u->itr_u != itr_u ) {
    return;
  }
  c3_free(itr_u);
  lis_u->itr_u = NULL;
}
