//! @file list.c

#include "c/list.h"

//==============================================================================
// Static functions
//==============================================================================

static c3_list_node*
_create_node(const void* const dat_v, const size_t siz_i)
{
  c3_list_node* nod_u = c3_calloc(sizeof(*nod_u) + siz_i);
  nod_u->len_i        = siz_i;
  memcpy(nod_u->dat_y, dat_v, siz_i);
  return nod_u;
}

//==============================================================================
// Functions
//==============================================================================

c3_list*
c3_list_init(void)
{
  return c3_calloc(sizeof(c3_list));
}

void
c3_list_push(c3_list* const    lis_u,
             const c3_list_end end_i,
             const void* const dat_v,
             const size_t      siz_i)
{
  c3_list_node* nod_u = _create_node(dat_v, siz_i);

  if ( 0 == c3_list_len(lis_u) ) {
    lis_u->fro_u = lis_u->bak_u = nod_u;
  }
  else {
    if ( C3_LIST_FRONT == end_i ) {
      lis_u->fro_u->pre_u = nod_u;
      nod_u->nex_u        = lis_u->fro_u;
      lis_u->fro_u        = nod_u;
    }
    else {
      lis_u->bak_u->nex_u = nod_u;
      nod_u->pre_u        = lis_u->bak_u;
      lis_u->bak_u        = nod_u;
    }
  }
  lis_u->len_i++;
}

c3_list_node*
c3_list_peek(const c3_list* const lis_u, const c3_list_end end_i)
{
  if ( 0 == c3_list_len(lis_u) ) {
    return NULL;
  }
  return (C3_LIST_FRONT == end_i) ? lis_u->fro_u : lis_u->bak_u;
}

c3_list_node*
c3_list_pop(c3_list* const lis_u, const c3_list_end end_i)
{
  if ( 0 == c3_list_len(lis_u) ) {
    return NULL;
  }

  c3_list_node* nod_u;
  if ( C3_LIST_FRONT == end_i ) {
    nod_u        = lis_u->fro_u;
    lis_u->fro_u = lis_u->fro_u->nex_u;
    if ( !lis_u->fro_u ) {
      lis_u->bak_u = NULL;
    }
    else {
      lis_u->fro_u->pre_u = NULL;
    }
  }
  else {
    nod_u        = lis_u->bak_u;
    lis_u->bak_u = lis_u->bak_u->pre_u;
    if ( !lis_u->bak_u ) {
      lis_u->fro_u = NULL;
    }
    else {
      lis_u->bak_u->nex_u = NULL;
    }
  }
  lis_u->len_i--;

  return nod_u;
}
