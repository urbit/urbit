        /* j/5/secp.c
**
*/
#include "all.h"
#include "urcrypt.h"
#include <ent.h>

static urcrypt_secp_context* sec_u;

/* call at process start */
void
u3je_secp_init()
{
  c3_y ent_y[32];
  ent_getentropy(ent_y, 32);
  sec_u = malloc(urcrypt_secp_prealloc_size());

  if ( 0 != urcrypt_secp_init(sec_u, ent_y) ) {
    u3l_log("u3e_secp_init failed\r\n");
    abort();
  }
}

/* call at process end */
void
u3je_secp_stop()
{
  urcrypt_secp_destroy(sec_u);
  free(sec_u);
  sec_u = NULL;
}

/* util funcs
 */
static c3_t
_cqes_in_order(u3_atom a)
{
  // this is the "n" parameter of the secp256k1 curve
  static const c3_w now_w[8] = {
    0xd0364141, 0xbfd25e8c, 0xaf48a03b, 0xbaaedce6,
    0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff
  };

  if ( 0 == a ) {
    return 0;
  }
  else if ( c3y == u3a_is_cat(a) ) {
    return 1;
  }
  else {
    u3a_atom* a_u = u3a_to_ptr(a);
    c3_w len_w = a_u->len_w;

    if ( len_w < 8 ) {
      return 1;
    }
    else if ( len_w > 8 ) {
      return 0;
    }
    else {
      c3_y i_y;
      c3_w *buf_w = a_u->buf_w;
      // loop from most to least significant words
      for ( i_y = 8; i_y > 0; ) {
        c3_w b_w = buf_w[i_y],
             o_w = now_w[--i_y];
        if ( b_w < o_w ) {
          return 1;
        }
        else if ( b_w > o_w ) {
          return 0;
        }
      }
      return 1;
    }
  }
}

static void
_cqes_unpack_fe(u3_atom k, c3_y out_y[32])
{
  if ( _cqes_in_order(k) ) {
    u3r_bytes(0, 32, out_y, k);
  }
  else {
    u3m_bail(c3__exit);
  }
}

/* sign hash with priv key
 */
static u3_noun
_cqes_sign(u3_atom has,
           u3_atom prv)
{
  c3_y has_y[32];

  if ( 0 != u3r_bytes_fit(32, has_y, has) ) {
    return u3m_bail(c3__exit);
  }
  else {
    c3_y prv_y[32], v_y, r_y[32], s_y[32];
    _cqes_unpack_fe(prv, prv_y);

    return( 0 == urcrypt_secp_sign(sec_u, has_y, prv_y, &v_y, r_y, s_y) )
      ? u3nt(v_y, u3i_bytes(32, r_y), u3i_bytes(32, s_y))
      : u3_none;
  }
}

u3_noun
u3we_sign(u3_noun cor)
{

  u3_noun has, prv;

  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2,  &has,
                        u3x_sam_3,  &prv,
                        0)) ||
       (c3n == u3ud(has)) ||
       (c3n == u3ud(prv))) {
    return u3m_bail(c3__exit);
  }
  else {
    return u3l_punt("secp-sign", _cqes_sign(has, prv));
  }
}

/* recover pubkey from signature (which is how we verify signatures)
*/
static u3_noun
_cqes_reco(u3_atom has,
           u3_atom siv,  /* signature: v */
           u3_atom sir,  /* signature: r */
           u3_atom sis)  /* signature: s */
{
  c3_y has_y[32];
  if ( !((siv < 4) && (0 == u3r_bytes_fit(32, has_y, has)) ) ) {
    return u3m_bail(c3__exit);
  }
  else {
    c3_y sir_y[32], sis_y[32], x_y[32], y_y[32];
    c3_y siv_y = (c3_y) siv;
    _cqes_unpack_fe(sir, sir_y);
    _cqes_unpack_fe(sis, sis_y);
    return
      ( 0 == urcrypt_secp_reco(sec_u, has_y, siv, sir_y, sis_y, x_y, y_y) )
      ? u3nc(u3i_bytes(32, x_y), u3i_bytes(32, y_y))
      : u3_none;
  }
}

u3_noun
u3we_reco(u3_noun cor)
{
  u3_noun has,      /* hash */
    siv, sir, sis;  /* signature: v, r, s */

  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2,   &has,
                        u3x_sam_6,   &siv,
                        u3x_sam_14,  &sir,
                        u3x_sam_15,  &sis,
                        0)) ||
       (c3n == u3ud(has)) ||
       (c3n == u3ud(siv)) ||
       (c3n == u3ud(sir)) ||
       (c3n == u3ud(sis)) ) {
    return u3m_bail(c3__exit);
  }
  else {
    return u3l_punt("secp-reco", _cqes_reco(has, siv, sir, sis));
  }
}

static u3_atom
_cqes_make(u3_atom has,
          u3_atom prv)
{
  c3_y has_y[32];

  if ( 0 != u3r_bytes_fit(32, has_y, has) ) {
    return u3m_bail(c3__exit);
  }
  else {
    c3_y prv_y[32], out_y[32];
    _cqes_unpack_fe(prv, prv_y);
    return ( 0 == urcrypt_secp_make(has_y, prv_y, out_y) )
      ? u3i_bytes(32, out_y)
      : u3_none;
  }
}

u3_noun
u3we_make(u3_noun cor)
{
  u3_noun has, prv;
  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2,  &has,
                        u3x_sam_3,  &prv,
                        0)) ||
       (c3n == u3ud(has)) ||
       (c3n == u3ud(prv)) ) {
    return u3m_bail(c3__exit);
  }
  else {
    return u3l_punt("secp-make", _cqes_make(has, prv));
  }
}

/* create a schnorr signature
*/
static u3_weak
_cqes_sosi(u3_atom sk, u3_atom m, u3_atom a)
{
  c3_y key_y[32];
  c3_y mes_y[32];
  c3_y aux_y[32];

  if ( (0 != u3r_bytes_fit(32, key_y, sk)) ||
       (0 != u3r_bytes_fit(32, mes_y, m)) ||
       (0 != u3r_bytes_fit(32, aux_y, a)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    c3_y sig_y[64];

    return
      ( 0 == urcrypt_secp_schnorr_sign(sec_u, key_y, mes_y, aux_y, sig_y) )
      ? u3i_bytes(64, sig_y)
      : u3_none;
  }
}

u3_noun
u3we_sosi(u3_noun cor)
{
  u3_noun key, mes, aux;

  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2,  &key,
                        u3x_sam_6,  &mes,
                        u3x_sam_7,  &aux,
                        0)) ||
       (c3n == u3ud(key)) ||
       (c3n == u3ud(mes)) ||
       (c3n == u3ud(aux)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return u3l_punt("secp-sosi", _cqes_sosi(key, mes, aux));
  }
}

/* verify a schnorr signature
*/
static u3_atom
_cqes_sove(u3_atom pk, u3_atom m, u3_atom sig)
{
  c3_y pub_y[32];
  c3_y mes_y[32];
  c3_y sig_y[64];

  if ( (0 != u3r_bytes_fit(32, pub_y, pk)) ||
       (0 != u3r_bytes_fit(32, mes_y, m)) ||
       (0 != u3r_bytes_fit(64, sig_y, sig)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return __(urcrypt_secp_schnorr_veri(sec_u, sig_y, mes_y, pub_y));
  }
}

u3_noun
u3we_sove(u3_noun cor)
{
  u3_noun pub, mes, sig;

  if ( (c3n == u3r_mean(cor,
                        u3x_sam_2,  &pub,
                        u3x_sam_6,  &mes,
                        u3x_sam_7,  &sig,
                        0)) ||
       (c3n == u3ud(pub)) ||
       (c3n == u3ud(mes)) ||
       (c3n == u3ud(sig)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return _cqes_sove(pub, mes, sig);
  }
}
