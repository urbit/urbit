//! @file http.c

#include "vere/io/http/http.h"

#include "c/defs.h"
#include "c/motes.h"
#include "noun/allocate.h"
#include "noun/imprison.h"
#include "noun/retrieve.h"
#include "noun/xtract.h"

//==============================================================================
// Functions
//==============================================================================

c3_t
u3_http_hed_new(u3_atom nam, u3_atom val, u3_hhed* const hed_u)
{
  if ( !hed_u ) {
    return 0;
  }

  c3_w nam_w = u3r_met(3, nam);
  c3_w val_w = u3r_met(3, val);

  *hed_u = (u3_hhed){
    .nam_w = nam_w,
    .nam_c = c3_malloc(nam_w + 1),
    .val_w = val_w,
    .val_c = c3_malloc(val_w + 1),
  };

  hed_u->nam_c[nam_w] = '\0';
  u3r_bytes(0, nam_w, (c3_y*)hed_u->nam_c, nam);

  hed_u->val_c[val_w] = '\0';
  u3r_bytes(0, val_w, (c3_y*)hed_u->val_c, val);

  return 1;
}

c3_list* const
u3_http_heds_to_list(u3_noun hed)
{
  u3_noun  deh   = hed;
  c3_list* lis_u = c3_list_init();
  if ( !lis_u ) {
    goto end;
  }

  while ( u3_nul != hed ) {
    u3_noun i_hed  = u3h(hed);
    u3_hhed hed_u;
    c3_assert(u3_http_hed_new(u3h(i_hed), u3t(i_hed), &hed_u));
    c3_list_pushb(lis_u, &hed_u, sizeof(hed_u));
    hed          = u3t(hed); 
  }

end:
  u3z(deh);
  return lis_u;
}

u3_noun
u3_http_heds_to_noun(h2o_header_t* hed_u, c3_d hed_d)
{
  u3_noun hed   = u3_nul;
  for ( c3_d idx_d = 0; idx_d < hed_d; idx_d++ ) {
    h2o_header_t deh_u = hed_u[hed_d - (idx_d + 1)];
    hed = u3nc(u3nc(u3_http_vec_to_atom(deh_u.name),
                    u3_http_vec_to_atom(&deh_u.value)),
               hed);
  }
  return hed;
}

void
u3_http_heds_free(c3_list* const hed_u)
{
  if ( !hed_u ) {
    return;
  }

  c3_lode* nod_u = c3_list_popf(hed_u);
  while ( nod_u ) {
    c3_free(nod_u);
    nod_u = c3_list_popf(hed_u);
  }
  c3_free(hed_u);
}

u3_hbod*
u3_http_bod_from_octs(u3_noun oct)
{
  if ( c3n == u3a_is_cat(u3h(oct)) ) { // 2GB max
    u3m_bail(c3__fail);
    return NULL;
  }
  c3_w len_w = u3h(oct);

  u3_hbod* bod_u      = c3_malloc(sizeof(*bod_u) + len_w + 1);
  bod_u->hun_y[len_w] = 0;
  bod_u->len_w        = len_w;
  bod_u->nex_u        = NULL;
  u3r_bytes(0, len_w, bod_u->hun_y, u3t(oct));
  u3z(oct);
  return bod_u;
}

void
u3_http_bods_to_vec(u3_hbod* bod_u, h2o_iovec_t** arr_u, c3_w* arr_w)
{
  c3_w len_w = 0;
  {
    u3_hbod* bid_u = bod_u;
    while ( bid_u ) {
      len_w++;
      bid_u = bid_u->nex_u;
    }
  }

  h2o_iovec_t* vec_u = NULL;
  if ( 0 < len_w ) {
    vec_u = c3_malloc(sizeof(h2o_iovec_t) * len_w);
    for ( c3_w idx_w = 0; idx_w < len_w; idx_w++ ) {
      vec_u[idx_w] = h2o_iovec_init(bod_u->hun_y, bod_u->len_w);
      bod_u = bod_u->nex_u;
    }
  }
  *arr_u = vec_u;
  *arr_w = len_w;
}

u3_noun
u3_http_vec_to_atom(h2o_iovec_t* vec_u)
{
  return u3i_bytes(vec_u->len, (const c3_y*)vec_u->base);
}

void
u3_http_bods_free(u3_hbod* bod_u)
{
  while ( bod_u ) {
    u3_hbod* nex_u = bod_u->nex_u;
    c3_free(bod_u);
    bod_u = nex_u;
  }
}
