//! @file queue.c

#include "c/queue.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static c3_node*
_create_node(void* dat_v, size_t siz_i)
{
  c3_node* nod_u = malloc(sizeof(c3_node));
  assert(nod_u);
  nod_u->dat_v = malloc(siz_i);
  memcpy(nod_u->dat_v, dat_v, siz_i);
  nod_u->siz_i = siz_i;
  nod_u->nex_u = NULL;
  nod_u->pre_u = NULL;
  return nod_u;
}

c3_queue*
c3_queue_init(void)
{
  c3_queue* que_u = calloc(1, sizeof(c3_queue));
  if ( NULL == que_u ) {
    return NULL;
  }

  return que_u;
}

size_t
c3_queue_length(const c3_queue* que_u)
{
  return (NULL == que_u) ? 0 : que_u->len_i;
}

c3_node*
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

  return nod_u;
}

c3_node*
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

  return nod_u;
}

c3_node*
c3_queue_peek_back(const c3_queue* que_u)
{
  return (NULL == que_u) ? NULL : que_u->las_u;
}

c3_node*
c3_queue_peek_front(const c3_queue* que_u)
{
  return (NULL == que_u) ? NULL : que_u->fir_u;
}

c3_node*
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

  return nod_u;
}

c3_node*
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

  return nod_u;
}

void
c3_queue_free(c3_queue* que_u)
{
  if ( NULL == que_u ) {
    return;
  }

  c3_node* nod_u;
  while ( NULL != (nod_u = c3_queue_pop_front(que_u)) ) {
    free(nod_u->dat_v);
    free(nod_u);
  }

  free(que_u);
}
