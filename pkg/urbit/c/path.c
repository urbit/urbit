//! @file path.c

#include "c/path.h"

#include "c/defs.h"

//==============================================================================
// Constants
//==============================================================================

//! Path separator.
static const c3_c pax_sep_c[] = "/";

//==============================================================================
// Static functions
//==============================================================================

static inline c3_t
_is_empty_token(const c3_c* const tok_c)
{
  return !tok_c || 0 == strcmp("", tok_c) || 0 == strcmp(".", tok_c);
}

static inline c3_t
_is_root(const c3_c* const tok_c)
{
  return 0 == strcmp(pax_sep_c, tok_c);
}

static inline c3_t
_is_parent(const c3_c* const tok_c)
{
  return 0 == strcmp("..", tok_c);
}

//==============================================================================
// Functions
//==============================================================================

//! @n (1) Allocate 32 bytes for the path string as a default.
c3_path*
c3_path_fa(const c3_c** tok_c, const size_t tok_i)
{
  static const size_t cap_i = 32; // (1)
  c3_path*            pax_u = c3_calloc(sizeof(*pax_u) + cap_i);
  pax_u->cap_i              = cap_i;
  if ( tok_c ) {
    for ( size_t idx_i = 0; idx_i < tok_i; idx_i++ ) {
      c3_path_push(pax_u, tok_c[idx_i]);
    }
  }
  return pax_u;
}

c3_path*
c3_path_fp(const c3_path* const pax_u)
{
  return pax_u ? c3_path_fv(1, pax_u->str_c) : c3_path_fv(0);
}

c3_path*
c3_path_fv(const size_t tok_i, ...)
{
  if ( 0 == tok_i ) {
    return c3_path_fa(NULL, 0);
  }
  const c3_c* tok_c[tok_i];
  va_list     arg_u;
  va_start(arg_u, tok_i);
  for ( size_t idx_i = 0; idx_i < tok_i; idx_i++ ) {
    tok_c[idx_i] = va_arg(arg_u, const c3_c*);
  }
  va_end(arg_u);
  return c3_path_fa(tok_c, tok_i);
}

//! @n (1) The root directory is its own parent.
//! @n (2) Double the needed size to minimize future allocations.
void
c3_path_push(c3_path* pax_u, const c3_c* const tok_c)
{
  if ( !pax_u || _is_empty_token(tok_c) ) {
    return;
  }

  if ( _is_parent(tok_c) ) {
    if ( !_is_root(pax_u->str_c) ) { // (1)
      c3_path_pop(pax_u);
    }
    return;
  }

  size_t tok_i = sizeof(pax_sep_c) + strlen(tok_c);
  if ( pax_u->cap_i - pax_u->len_i < tok_i ) {
    size_t cap_i = 2 * (pax_u->cap_i + tok_i); // (2)
    pax_u        = c3_realloc(pax_u, sizeof(*pax_u) + cap_i);
    pax_u->cap_i = cap_i;
  }

  if ( 0 < pax_u->len_i && !_is_root(pax_u->str_c) ) {
    strcat(pax_u->str_c, pax_sep_c);
  }
  strcat(pax_u->str_c, tok_c);

  pax_u->len_i = strlen(pax_u->str_c);
}

//! @n (1) This is a path of the form `some-file-or-directory`.
//! @n (2) This is a path of the form `/some-file-or-directory`.
void
c3_path_pop(c3_path* const pax_u)
{
  if ( !pax_u || 0 == pax_u->len_i ) {
    return;
  }

  if ( _is_root(pax_u->str_c) ) {
    *pax_u->str_c = '\0';
    pax_u->len_i  = 0;
    return;
  }

  c3_c* sep_c = strrchr(pax_u->str_c, *pax_sep_c);
  if ( !sep_c ) { // (1)
    sep_c = pax_u->str_c;
  }
  else if ( sep_c == pax_u->str_c ) { // (2)
    sep_c++;
  }
  *sep_c       = '\0';
  pax_u->len_i = strlen(pax_u->str_c);
}
