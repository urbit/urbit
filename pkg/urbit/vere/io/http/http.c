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

u3_noun
u3_http_vec_to_atom(h2o_iovec_t* vec_u)
{
  return u3i_bytes(vec_u->len, (const c3_y*)vec_u->base);
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

void
u3_http_bods_free(u3_hbod* bod_u)
{
  while ( bod_u ) {
    u3_hbod* nex_u = bod_u->nex_u;
    c3_free(bod_u);
    bod_u = nex_u;
  }
}
