/// @file list.c

#include "c/list.h"

//==============================================================================
// Types
//==============================================================================

/// Doubly-linked list node. Typedefed to `c3_lode`.
struct _c3_list_node {
  /// Next node in the list.
  struct _c3_list_node* nex_u;

  /// Previous node in the list.
  struct _c3_list_node* pre_u;

  /// Length of `dat_y` in bytes.
  size_t dat_i;

  /// Payload data.
  c3_y dat_y[];
};

/// Doubly-linked list handle. Typedefed to `c3_list`.
struct _c3_list {
  /// Node at the front of the list.
  c3_lode* fro_u;

  /// Node at the back of the list.
  c3_lode* bak_u;

  /// Number of nodes in the list.
  size_t len_i;

  /// Ownership mode of the list.
  c3_list_ownership own_e;
};

//==============================================================================
// Static functions
//==============================================================================

static inline c3_lode*
_create_node(const void* const dat_v, size_t dat_i, c3_list_ownership own_e)
{
  c3_lode* nod_u = NULL;
  switch ( own_e ) {
    case C3_LIST_COPY:
      nod_u        = c3_calloc(sizeof(*nod_u) + dat_i);
      nod_u->dat_i = dat_i;
      memcpy(nod_u->dat_y, dat_v, dat_i);
      break;
    case C3_LIST_TRANSFER:
      nod_u        = c3_calloc(sizeof(*nod_u) + sizeof(dat_v));
      nod_u->dat_i = dat_i;
      memcpy(nod_u->dat_y, &dat_v, sizeof(dat_v));
      break;
    case C3_LIST_OWNERSHIP_END:
      break;
  }
  return nod_u;
}

//==============================================================================
// Functions
//==============================================================================

c3_list* const
c3_list_init(c3_list_ownership own_e)
{
  if ( own_e >= C3_LIST_OWNERSHIP_END ) {
    return NULL;
  }

  c3_list* const lis_u = c3_calloc(sizeof(*lis_u));
  lis_u->own_e         = own_e;

  return lis_u;
}

size_t
c3_list_len(const c3_list* const lis_u)
{
  return lis_u ? lis_u->len_i : 0;
}

void
c3_list_push(c3_list* const    lis_u,
             const c3_list_end end_i,
             const void* const dat_v,
             const size_t      siz_i)
{
  c3_lode* nod_u = _create_node(dat_v, siz_i, lis_u->own_e);

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

c3_lode*
c3_list_peek(const c3_list* const lis_u, const c3_list_end end_i)
{
  if ( 0 == c3_list_len(lis_u) ) {
    return NULL;
  }
  return (C3_LIST_FRONT == end_i) ? lis_u->fro_u : lis_u->bak_u;
}

c3_lode*
c3_list_pop(c3_list* const lis_u, const c3_list_end end_i)
{
  if ( 0 == c3_list_len(lis_u) ) {
    return NULL;
  }

  c3_lode* nod_u;
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

c3_lode*
c3_lode_next(const c3_lode* const nod_u)
{
  return nod_u ? nod_u->nex_u : NULL;
}

c3_lode*
c3_lode_prev(const c3_lode* const nod_u)
{
  return nod_u ? nod_u->pre_u : NULL;
}

void*
c3_lode_data(const c3_lode* const nod_u)
{
  return nod_u ? (void*)nod_u->dat_y : NULL;
}

size_t
c3_lode_len(const c3_lode* const nod_u)
{
  return nod_u ? nod_u->dat_i : 0;
}
