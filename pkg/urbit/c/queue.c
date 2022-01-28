//! @file queue.c

#include "c/queue.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c/types.h"
#include "c/defs.h"

//! Single element of a doubly-linked list.
typedef struct _c3_node {
  void*            dat_v;  //!< data
  struct _c3_node* nex_u;  //!< next node
  struct _c3_node* pre_u;  //!< previous node
} c3_node;

//! Queue abstraction backed by a doubly-linked list.
struct _c3_queue {
  c3_node* fir_u;  //!< first node in queue
  c3_node* las_u;  //!< last node in queue
  size_t   len_i;  //!< length of queue in nodes
};

static c3_node*
_create_node(void* dat_v, size_t siz_i)
{
  c3_node* nod_u = c3_malloc(sizeof(c3_node));
  assert(nod_u);
  nod_u->dat_v = c3_malloc(siz_i);
  memcpy(nod_u->dat_v, dat_v, siz_i);
  nod_u->nex_u = NULL;
  nod_u->pre_u = NULL;
  return nod_u;
}

c3_queue*
c3_queue_init(void)
{
  c3_queue* que_u = c3_malloc(sizeof(c3_queue));
  if ( NULL == que_u ) {
    return NULL;
  }

  que_u->fir_u = NULL;
  que_u->las_u = NULL;
  que_u->len_i = 0;

  return que_u;
}

size_t
c3_queue_length(const c3_queue* que_u)
{
  return (NULL == que_u) ? 0 : que_u->len_i;
}

void*
c3_queue_push_back(c3_queue* que_u, void* dat_v, size_t siz_i)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  c3_node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_queue_length(que_u) ) {
    assert(NULL == que_u->fir_u && NULL == que_u->las_u);
    que_u->fir_u = nod_u;
    que_u->las_u = nod_u;
  }
  else {
    assert(NULL != que_u->las_u);
    que_u->las_u->nex_u = nod_u;
    nod_u->pre_u = que_u->las_u;
    que_u->las_u = nod_u;
  }
  que_u->len_i++;

  return nod_u->dat_v;
}

void*
c3_queue_push_front(c3_queue* que_u, void* dat_v, size_t siz_i)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  c3_node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_queue_length(que_u) ) {
    assert(NULL == que_u->fir_u && NULL == que_u->las_u);
    que_u->fir_u = nod_u;
    que_u->las_u = nod_u;
  }
  else {
    assert(NULL != que_u->fir_u);
    que_u->fir_u->pre_u = nod_u;
    nod_u->nex_u = que_u->fir_u;
    que_u->fir_u = nod_u;
  }
  que_u->len_i++;

  return nod_u->dat_v;
}

void*
c3_queue_peek_back(const c3_queue* que_u)
{
  return (0 == c3_queue_length(que_u)) ? NULL : que_u->las_u->dat_v;
}

void*
c3_queue_peek_front(const c3_queue* que_u)
{
  return (0 == c3_queue_length(que_u)) ? NULL : que_u->fir_u->dat_v;
}

void*
c3_queue_pop_back(c3_queue* que_u)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  if ( NULL == que_u->las_u ) {
    assert(NULL == que_u->fir_u);
    return NULL;
  }

  c3_node* nod_u = que_u->las_u;
  que_u->las_u = que_u->las_u->pre_u;
  if ( NULL == que_u->las_u ) {
    que_u->fir_u = NULL;
  }
  else {
    que_u->las_u->nex_u = NULL;
  }
  nod_u->pre_u = NULL;
  que_u->len_i--;

  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);

  return dat_v;
}

void*
c3_queue_pop_front(c3_queue* que_u)
{
  if ( NULL == que_u ) {
    return NULL;
  }

  if ( NULL == que_u->fir_u ) {
    assert(NULL == que_u->las_u);
    return NULL;
  }

  c3_node* nod_u = que_u->fir_u;
  que_u->fir_u = que_u->fir_u->nex_u;
  if ( NULL == que_u->fir_u ) {
    que_u->las_u = NULL;
  }
  else {
    que_u->fir_u->pre_u = NULL;
  }
  nod_u->nex_u = NULL;
  que_u->len_i--;

  void* dat_v = nod_u->dat_v;
  c3_free(nod_u);

  return dat_v;
}

void
c3_queue_free(c3_queue* que_u)
{
  if ( NULL == que_u ) {
    return;
  }

  c3_node* nod_u;
  while ( NULL != (nod_u = c3_queue_pop_front(que_u)) ) {
    c3_free(nod_u->dat_v);
    c3_free(nod_u);
  }

  c3_free(que_u);
}
