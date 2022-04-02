#include "urcrypt.h"
#include "util.h"
#include <string.h>
#include <secp256k1.h>
#include <secp256k1_extrakeys.h>
#include <secp256k1_recovery.h>
#include <secp256k1_preallocated.h>
#include <secp256k1_schnorrsig.h>

#define SECP_FLAGS SECP256K1_CONTEXT_VERIFY | SECP256K1_CONTEXT_SIGN

struct urcrypt_secp_context_struct {
  secp256k1_context* secp;
  uint8_t prealloc[];
};

size_t
urcrypt_secp_prealloc_size()
{
  return sizeof(urcrypt_secp_context) +
    secp256k1_context_preallocated_size(SECP_FLAGS);
}

int
urcrypt_secp_init(urcrypt_secp_context *context,
                  uint8_t entropy[32])
{
  secp256k1_context* secp =
    secp256k1_context_preallocated_create(context->prealloc, SECP_FLAGS);
  if ( 1 == secp256k1_context_randomize(secp, entropy) ) {
    context->secp = secp;
    return 0;
  }
  else {
    secp256k1_context_preallocated_destroy(secp);
    return -1;
  }
}

void
urcrypt_secp_destroy(urcrypt_secp_context *context)
{
  secp256k1_context_preallocated_destroy(context->secp);
}

int
urcrypt_secp_make(uint8_t hash[32], uint8_t key[32], uint8_t out[32])
{
  urcrypt__reverse(32, hash);
  urcrypt__reverse(32, key);

  if ( 1 != secp256k1_nonce_function_rfc6979(
    out,   // OUT: return arg for nonce
    hash,  // IN: message / hash */
    key,   // IN: key32
    NULL,  // IN: algorithm (NULL == ECDSA)
    NULL,  // IN: arbitrary data pointer (unused)
    0) ) { // IN: attempt number (0 == normal)
    return -1;
  }
  else {
    urcrypt__reverse(32, out);
    return 0;
  }
}

int
urcrypt_secp_sign(urcrypt_secp_context* context,
                  uint8_t hash[32],
                  uint8_t key[32],
                  uint8_t* out_v,
                  uint8_t out_r[32],
                  uint8_t out_s[32])
{
  secp256k1_ecdsa_recoverable_signature signature;

  urcrypt__reverse(32, hash);
  urcrypt__reverse(32, key);

  /* sign
     N.B. if we want the 'v' field we can't use default secp256k1_ecdsa_sign(),
     but must use secp256k1_ecdsa_sign_recoverable() */
  if ( 1 != secp256k1_ecdsa_sign_recoverable(
        context->secp, /* IN: context object */
        &signature,    /* OUT: signature */
        hash,          /* IN: 32 byte hash to be signed */
        key,           /* IN: 32 byte secret key */
        NULL,          /* IN: nonce-function ptr ; NULL = default */
        NULL) ) {      /* IN: data for nonce function; not used  */
    return -1;
  }
  else {
    uint8_t sigbytes[64];
    int recid;
    if ( 1 != secp256k1_ecdsa_recoverable_signature_serialize_compact(
        context->secp,  /* IN: context object */
        sigbytes,       /* OUT: 64 byte sig (r,s) */
        &recid,         /* OUT: v */
        &signature) ) { /* IN:  65 byte sig */
      return -2;
    }
    else {
      /* read sigbytes into r and s
         convert endianness while we're at it */
      uint8_t i, j;
      for ( j = 31, i = 0; i < 32; ++i, --j) {
        out_r[j] = sigbytes[i];
      }
      for ( j = 31; i < 64; ++i, --j ) {
        out_s[j] = sigbytes[i];
      }
      *out_v = (uint8_t) recid;
      return 0;
    }
  }
}

int
urcrypt_secp_reco(urcrypt_secp_context* context,
                  uint8_t hash[32],
                  uint8_t key_v,
                  const uint8_t key_r[32],
                  const uint8_t key_s[32],
                  uint8_t out_x[32],
                  uint8_t out_y[32])
{
  if ( (NULL == hash) ||
       (NULL == key_r) ||
       (NULL == key_s) ) {
    return -1;
  }
  else if ( key_v > 3 ) {
    return -2;
  }
  else {
    secp256k1_ecdsa_recoverable_signature signature;
    uint8_t private[64];
    uint8_t i, j;
    // make big private key out of two smaller parts, reversing endianness
    for ( j = 31, i = 0; i < 32; ++i, --j) {
      private[i] = key_r[j];
    }
    for ( j = 31; i < 64; ++i, --j ) {
      private[i] = key_s[j];
    }
    memset(&signature, 0, sizeof(secp256k1_ecdsa_recoverable_signature));
    if ( 1 != secp256k1_ecdsa_recoverable_signature_parse_compact(
          context->secp, /* IN:  context */
          &signature,    /* OUT: sig */
          private,       /* IN:  r/s */
          key_v) ) {     /* IN:  v */
      return -3;
    }
    else {
      secp256k1_pubkey public;
      memset(&public, 0, sizeof(secp256k1_pubkey));
      urcrypt__reverse(32, hash);
      if ( 1 != secp256k1_ecdsa_recover(
            context->secp, /* IN:  context */
            &public,       /* OUT: pub key */
            &signature,    /* IN: signature */
            hash) ) {      /* IN: message hash */
        return -4;
      }
      else {
        /* convert pub into serialized form that we can get x, y out of */
        uint8_t serialized[65];
        size_t outputlen = 65;
        memset(serialized, 0, outputlen);
        if ( 1 != secp256k1_ec_pubkey_serialize(
            context->secp,                 /* IN:  context */
            serialized,                    /* OUT: output */
            &outputlen,                    /* IN/OUT: outputlen */
            &public,                       /* IN: pubkey*/
            SECP256K1_EC_UNCOMPRESSED) ) { /* IN: flags */
          return -5;
        }
        else {
          /* in file
             subprojects/secp256k1/src/eckey_impl.h
             func
             secp256k1_eckey_pubkey_parse()
             we can see
             byte      0: signal bits (???)
             bytes  1-32: x
             bytes 33-64: y

             convert endianness while we're at it   */
          for (j = 32, i = 0; i < 32; ++i, --j) {
            out_x[i] = serialized[j];
          }
          for (j = 64, i = 0; i < 32; ++i, --j) {
            out_y[i] = serialized[j];
          }
          return 0;
        }
      }
    }
  }
}

int
urcrypt_secp_schnorr_sign(urcrypt_secp_context* context,
                          uint8_t key[32],
                          uint8_t msg[32],
                          uint8_t aux[32],
                          uint8_t out_sig[64])
{
  secp256k1_keypair keypair;

  urcrypt__reverse(32, key);
  urcrypt__reverse(32, msg);
  urcrypt__reverse(32, aux);

  if ( 1 != secp256k1_keypair_create(context->secp, &keypair, key) ) {
    return -1;
  }
  if ( 1 != secp256k1_schnorrsig_sign(context->secp, out_sig, msg, &keypair, aux) ) {
    return -1;
  }

  urcrypt__reverse(64, out_sig);
  return 0;
}

bool
urcrypt_secp_schnorr_veri(urcrypt_secp_context* context,
                          uint8_t sig[64],
                          uint8_t msg[32],
                          uint8_t pub[32])
{
  secp256k1_xonly_pubkey pubkey;

  urcrypt__reverse(64, sig);
  urcrypt__reverse(32, msg);
  urcrypt__reverse(32, pub);

  if ( 1 != secp256k1_xonly_pubkey_parse(context->secp, &pubkey, pub) ) {
    return false;
  }
  if ( 1 != secp256k1_schnorrsig_verify(context->secp, sig, msg, 32, &pubkey) ) {
    return false;
  }
  return true;
}
