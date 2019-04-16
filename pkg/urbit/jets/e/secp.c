        /* j/5/secp.c
**
*/
#include "all.h"
#include "../include/secp256k1.h"
#include "../include/secp256k1_recovery.h"

/* util funcs
 */

/* no guarantees if 'in' and 'out' overlap / are the same */
static void byte_reverse(c3_y *i_y,  /* in */
                         c3_y *o_y,  /* out */
                         c3_w  n_w)   /* size */
{
  c3_w j_w;
  for (j_w = 0; j_w < n_w; j_w++){
    o_y[n_w - 1 - j_w] = i_y[j_w];
  }

  return;
}

/* Identical to u3r_bytes, but reverses bytes in place.
   could be cleaner if we modified u3r_bytes(), but not gonna do that.

   This func exists bc Urbit code base is explicitly little-endian,
   and secp256k1 library is explicitly big-endian.

   Several times below we do the pattern of (1) invoke u3r_bytes, (2) invert. Do it in a func.
*/


static void u3r_bytes_reverse(c3_w    a_w,
                              c3_w    b_w,
                              c3_y*   c_y,  /* out */
                              u3_atom d)    /* in */
{
  u3r_bytes(a_w, b_w, c_y, d);
  c3_w i_w;
  for (i_w = 0; i_w < ((b_w - a_w) / 2) ; i_w++) {
    c3_y lo = c_y[i_w];
    c3_y hi = c_y[b_w - i_w - 1];
    c_y[i_w] = hi;
    c_y[b_w - i_w - 1] = lo;
  }

  return;
}


/* sign hash with priv key
 */

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
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  } else {
    return (u3qe_sign(has, prv));
  }
}

u3_noun
u3qe_sign(u3_atom has,
          u3_atom prv)
{
  /* build library context object once (and only once) */
  static secp256k1_context * ctx_u = NULL;
  if (NULL == ctx_u) {
    ctx_u = secp256k1_context_create(SECP256K1_CONTEXT_SIGN);
  }

  /* parse arguments, convert endianness */
  c3_y has_y[32];   /* hash */
  c3_y prv_y[32];   /* private key */
  u3r_bytes_reverse(0, 32, has_y, has);
  u3r_bytes_reverse(0, 32, prv_y, prv);


  /* sign
     N.B. if we want the 'v' field we can't use default secp256k1_ecdsa_sign(),
     but must use secp256k1_ecdsa_sign_recoverable() */
  c3_ws ret;
  secp256k1_ecdsa_recoverable_signature sig_u;
  ret = secp256k1_ecdsa_sign_recoverable(ctx_u,                            /* IN: context object */
                                         & sig_u,                          /* OUT: signature */
                                         (const c3_y *) has_y,             /* IN: 32 byte hash to be signed */
                                         (const c3_y *) prv_y,             /* IN: 32 byte secret key */
                                         (secp256k1_nonce_function) NULL,  /* IN: nonce-function ptr ; NULL = default */
                                         (const void *) NULL);             /* IN: data for nonce function; not used  */
  if (1 != ret) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  /* convert opaque 65 byte signature into v + [r + s]
     convert endianness while we're at it */
  c3_y rec_y[64];
  c3_ws v = 0;
  ret = secp256k1_ecdsa_recoverable_signature_serialize_compact(ctx_u,
                                                                rec_y,    /* OUT: 64 byte sig (r,s) */
                                                                & v,      /* OUT: v */
                                                                & sig_u); /* IN:  65 byte sig */
  if (1 != ret) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  c3_y s_y[32];
  c3_y r_y[32];
  byte_reverse(rec_y,      r_y, 32);
  byte_reverse(rec_y + 32, s_y, 32);

  /* package s,r,v signature for return */
  u3_noun s  = u3i_words(8, (const c3_w*) s_y);
  u3_noun r  = u3i_words(8, (const c3_w*) r_y);
  return (u3nt(v, r, s));
}


/* recover pubkey from signature (which is how we verify signatures)
*/

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
       (c3n == u3ud(sis)) )
    {
      u3l_log("\rsecp jet: crypto package error\n");
      return u3m_bail(c3__exit);
    } else {
    return u3qe_reco(has, siv, sir, sis);
  }
}

u3_noun
u3qe_reco(u3_atom has,
          u3_atom siv,  /* signature: v */
          u3_atom sir,  /* signature: r */
          u3_atom sis)  /* signature: s */
{

  /* build library context object once (and only once) */
  static secp256k1_context * ctx_u = NULL;
  if (NULL == ctx_u) {
    ctx_u = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY);
  }

  /* parse arguments, convert endianness */
  c3_y has_y[32];
  c3_y sir_y[32];
  c3_y sis_y[32];
  c3_y siv_y[1];
  u3r_bytes_reverse(0, 32, has_y, has);
  u3r_bytes_reverse(0, 32, sir_y, sir);
  u3r_bytes_reverse(0, 32, sis_y, sis);
  u3r_bytes_reverse(0, 1,  siv_y, siv);

  /* build the signature object */
  c3_y ras_y[64];  /*  priv key: r and s components */
  c3_ws i_ws;
  for (i_ws = 0; i_ws < 32; i_ws++) {
    ras_y[i_ws] = sir_y[i_ws] ;
  }
  for (i_ws = 0; i_ws < 32; i_ws++) {
    ras_y[i_ws + 32] = sis_y[i_ws] ;
  }

  c3_ws siv_ws = siv_y[0];
  secp256k1_ecdsa_recoverable_signature sig_u;
  memset( (void *)  & sig_u, 0, sizeof(secp256k1_ecdsa_recoverable_signature)  );
  c3_ws ret = secp256k1_ecdsa_recoverable_signature_parse_compact(ctx_u,     /* IN:  context */
                                                                  & sig_u,   /* OUT: sig */
                                                                  ras_y,     /* IN:  r/s */
                                                                  siv_ws);   /* IN:  v */
  if (1 != ret) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  /* turn sign into puk_u */
  secp256k1_pubkey puk_u;
  memset((void *) & puk_u, 0, sizeof(secp256k1_pubkey) );
  ret = secp256k1_ecdsa_recover(ctx_u,                      /* IN: context */
                                & puk_u,                     /* OUT: pub key */
                                & sig_u,                        /* IN: signature */
                                (const c3_y *) & has);        /* IN: message has */

  if (1 != ret) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  /* convert puk_u  into serialized form that we can get x,y out of */
  c3_y puk_y[65];
  size_t outputlen = 65;
  memset((void *) puk_y, 0, 65);

  ret = secp256k1_ec_pubkey_serialize( ctx_u,                    /* IN:  */
                                      puk_y,                   /* OUT: */
                                      & outputlen,                /* OUT: */
                                      & puk_u,                   /* IN: */
                                      SECP256K1_EC_UNCOMPRESSED); /* IN: flags */

  if (1 != ret) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  /* in file
     subprojects/secp256k1/src/eckey_impl.h
     func
     secp256k1_eckey_puk_u_parse()
     we can see
     byte      0: signal bits (???)
     bytes  1-32: x
     bytes 33-64: y

     convert endianness while we're at it   */

  c3_y x_y[32];
  for (i_ws = 0; i_ws < 32; i_ws++) {
    x_y[i_ws] = puk_y[32 - i_ws];
  }
  u3_noun x =  u3i_bytes(32, x_y);

  c3_y y_y[32];
  for (i_ws = 0; i_ws < 32; i_ws++) {
    y_y[i_ws] = puk_y[64 - i_ws];
  }
  u3_noun y =  u3i_bytes(32, y_y);

  /* returns x,y */
  return(u3nc(x, y));
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
       (c3n == u3ud(prv)) )
    {
      u3l_log("\rsecp jet: crypto package error\n");
      return u3m_bail(c3__exit);
    } else {
    return u3qe_make(has, prv);
  }
}



u3_noun
u3qe_make(u3_atom has,
          u3_atom prv)
{

  c3_y hel_y[32]; /* hash, little endian */
  c3_y heb_y[32]; /* hash, big endian */
  u3r_bytes(0, 32, hel_y, has);
  byte_reverse(hel_y, heb_y, 32);

  c3_y pel_y[32];  /* priv key, little endian */
  c3_y peb_y[32];  /* priv key, big endian */
  u3r_bytes(0, 32, pel_y, prv);
  byte_reverse(pel_y, peb_y, 32);


  c3_ws ret_ws;
  c3_y  neb_y[32]; /* nonce */
  ret_ws = secp256k1_nonce_function_rfc6979(neb_y,                /* OUT: return arg for nonce */
                                            (const c3_y *) heb_y, /* IN: message / hash */
                                            (const c3_y *) peb_y, /* IN: key32 */
                                            NULL,                 /* IN: algorithm (NULL == ECDSA) */
                                            (void *) NULL,        /* IN: arbitrary data pointer (unused) */
                                            0);                   /* IN: attempt number (0 == normal) */
  if (1 != ret_ws) {
    u3l_log("\rsecp jet: crypto package error\n");
    return u3m_bail(c3__exit);
  }

  c3_y nel_y[32];
  byte_reverse(neb_y, nel_y, 32);
  u3_noun non = u3i_words(8, (const c3_w*) nel_y);
  return(non);
}

