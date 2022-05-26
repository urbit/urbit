//! @file http.h
//! Functionality shared by client.c and server.c.

#ifndef U3_VERE_HTTP_H
#define U3_VERE_HTTP_H

#include <h2o.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/list.h"
#include "noun/aliases.h"

//==============================================================================
// Types
//==============================================================================

//! HTTP header.
typedef struct {
  c3_c* nam_c; //!< header key
  c3_w  nam_w; //!< length of `nam_c` in bytes
  c3_c* val_c; //!< header value
  c3_w  val_w; //!< length of `val_w` in bytes
} u3_hhed;

//! HTTP body block. Also used for responses.
typedef struct _u3_hbod {
  struct _u3_hbod* nex_u;
  c3_w             len_w;
  c3_y             hun_y[0];
} u3_hbod;

//==============================================================================
// Functions
//==============================================================================

//! Populate a new HTTP header from name and value cords.
//!
//! @param[in]  nam
//! @param[in]  val
//! @param[out] hed_u  HTTP header to populate.
//!
//! @return 0  `hed_u` was NULL.
//! @return 1  Successfully populated `hed_u`.
c3_t
u3_http_hed_new(u3_atom nam, u3_atom val, u3_hhed* const hed_u);

//! Convert h2o_iovec_t to atom (cord).
//!
//! @param[in] vec_u  h2o_iovec_t to convert.
//!
//! @return  atom
u3_noun
u3_http_vec_to_atom(h2o_iovec_t* vec_u);

//! Convert (list (pair @t @t)) to a list of HTTP headers.
//!
//! @param[in]  hed    Source HTTP header noun.
//!
//! @return  List of HTTP headers.
c3_list* const
u3_http_heds_to_list(u3_noun hed);

//! Convert h2o_header_t to (list (pair @t @t)).
//!
//! @param[in] hed_u  Array of h2o_header_t.
//! @param[in] hed_d  Number of elements in `hed_u`.
//!
//! @return  (list (pair @t @t))
u3_noun
u3_http_heds_to_noun(h2o_header_t* hed_u, c3_d hed_d);

//! Free a list of HTTP headers.
//!
//! @param[in] hed_u  List of HTTP headers to free.
void
u3_http_heds_free(c3_list* const hed_u);

//! Free a list of HTTP bodies.
//!
//! @param[in] bod_u  List of HTTP bodies to free.
void
u3_http_bods_free(u3_hbod* bod_u);

#endif /* ifndef U3_VERE_HTTP_H */
