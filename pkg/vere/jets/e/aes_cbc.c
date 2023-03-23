/* j/5/aes_cbc.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* All of the CBC hoon truncates its key and prv inputs by passing them to
 * the ECB functions, which truncate them, hence the raw u3r_bytes unpacking.
 */

typedef int (*urcrypt_cbc)(c3_y**,
                           size_t*,
                           c3_y*,
                           c3_y*,
                           urcrypt_realloc_t);

/* functions
*/
  static u3_atom
  _cqea_cbc_help(c3_y* key_y, u3_atom iv, u3_atom msg, urcrypt_cbc low_f)
  {
    u3_atom ret;
    c3_w    met_w;
    c3_y    iv_y[16];
    c3_y*   msg_y = u3r_bytes_all(&met_w, msg);
    size_t  len = met_w;

    u3r_bytes(0, 16, iv_y, iv);
    if ( 0 != (*low_f)(&msg_y, &len, key_y, iv_y, &u3a_realloc) ) {
      ret = u3_none;
    }
    else {
      ret = u3i_bytes(len, msg_y);
    }
    u3a_free(msg_y);

    return ret;
  }

  static u3_atom
  _cqea_cbca_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[16];
    u3r_bytes(0, 16, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbca_en);
  }

  u3_noun
  u3wea_cbca_en(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbca-en", _cqea_cbca_en(a, b, c));
    }
  }

  static u3_atom
  _cqea_cbca_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[16];
    u3r_bytes(0, 16, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbca_de);
  }

  u3_noun
  u3wea_cbca_de(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbca-de", _cqea_cbca_de(a, b, c));
    }
  }

  static u3_atom
  _cqea_cbcb_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[24];
    u3r_bytes(0, 24, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbcb_en);
  }

  u3_noun
  u3wea_cbcb_en(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbcb-en", _cqea_cbcb_en(a, b, c));
    }
  }

  static u3_atom
  _cqea_cbcb_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[24];
    u3r_bytes(0, 24, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbcb_de);
  }

  u3_noun
  u3wea_cbcb_de(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbcb-de", _cqea_cbcb_de(a, b, c));
    }
  }

  static u3_atom
  _cqea_cbcc_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbcc_en);
  }

  u3_noun
  u3wea_cbcc_en(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbcc-en", _cqea_cbcc_en(a, b, c));
    }
  }

  static u3_atom
  _cqea_cbcc_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_cbc_help(key_y, iv, msg, &urcrypt_aes_cbcc_de);
  }

  u3_noun
  u3wea_cbcc_de(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam, &c, 60, &a, 61, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("cbcc-de", _cqea_cbcc_de(a, b, c));
    }
  }
