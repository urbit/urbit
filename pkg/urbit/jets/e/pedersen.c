
/* j/5/pedersen.c
**
*/
#include "all.h"
#include <urcrypt.h>

static urcrypt_pedersen_context *cxt_u;

/* call at process start */
void
u3je_pedersen_init()
{
  cxt_u = malloc(urcrypt_pedersen_context_size());
  if ( 0 != urcrypt_pedersen_init(cxt_u) ) {
    u3l_log("u3je_pedersen_init failed\r\n");
    abort();
  }
}

/* call at process end */
void
u3je_pedersen_stop()
{
  urcrypt_pedersen_destroy(cxt_u);
  free(cxt_u);
  cxt_u = NULL;
}

static u3_noun
_cqe_phash(u3_atom a, u3_atom b)
{
  c3_y *a_buf_y, *b_buf_y, out_y[32];
  c3_w a_len_w, b_len_w, met_w, ret = u3_none;

  if ( u3_nul == a ) {
    met_w = a_len_w = 1;
  }
  else {
    met_w = a_len_w = u3r_met(3, a);
  }
  a_buf_y = u3r_bytes_alloc(0, met_w, a);

  if ( u3_nul == b ) {
    met_w = b_len_w = 1;
  }
  else {
    met_w = b_len_w = u3r_met(3, b);
  }
  b_buf_y = u3r_bytes_alloc(0, met_w, b);

  if ( 0 == urcrypt_pedersen(cxt_u, a_buf_y, a_len_w, b_buf_y, b_len_w, out_y) ) {
    ret = u3i_bytes(32, out_y);
  }
  u3a_free(a_buf_y);
  u3a_free(b_buf_y);
  return ret;
}

u3_noun
u3we_phash(u3_noun cor)
{
    u3_noun a, b;
    if ( (c3n == u3r_mean(cor,
                          u3x_sam_2,  &a,
                          u3x_sam_3,  &b,
                          0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
        return u3m_bail(c3__exit);
    }
    else {
        return _cqe_phash(a, b);
    }
}
