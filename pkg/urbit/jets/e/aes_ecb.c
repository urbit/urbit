/* j/5/aes_ecb.c
**
*/
#include "all.h"
#include <urcrypt.h>

typedef int (*urcrypt_ecb)(c3_y*, c3_y[16], c3_y[16]);

/* functions
*/

  /* All of the ECB hoon truncates its key and blk inputs with +fe, in these
   * jets we unpack with an unconditional u3r_bytes */

  static u3_atom
  _cqea_ecb_help(c3_y* key_y, u3_atom blk, urcrypt_ecb low_f)
  {
    c3_y blk_y[16], out_y[16];

    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != (*low_f)(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
  }

  static u3_atom
  _cqea_ecba_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16];
    u3r_bytes(0, 16, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecba_en);
  }

  u3_noun
  u3wea_ecba_en(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecba-en", _cqea_ecba_en(a, b));
    }
  }

  static u3_atom
  _cqea_ecba_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16];
    u3r_bytes(0, 16, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecba_de);
  }

  u3_noun
  u3wea_ecba_de(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecba-de", _cqea_ecba_de(a, b));
    }
  }

  static u3_atom
  _cqea_ecbb_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24];
    u3r_bytes(0, 24, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecbb_en);
  }

  u3_noun
  u3wea_ecbb_en(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecbb-en", _cqea_ecbb_en(a, b));
    }
  }

  static u3_atom
  _cqea_ecbb_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24];
    u3r_bytes(0, 24, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecbb_de);
  }

  u3_noun
  u3wea_ecbb_de(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecbb-de", _cqea_ecbb_de(a, b));
    }
  }

  static u3_atom
  _cqea_ecbc_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecbc_en);
  }

  u3_noun
  u3wea_ecbc_en(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecbc-en", _cqea_ecbc_en(a, b));
    }
  }

  static u3_atom
  _cqea_ecbc_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_ecb_help(key_y, blk, &urcrypt_aes_ecbc_de);
  }

  u3_noun
  u3wea_ecbc_de(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("ecbc-de", _cqea_ecbc_de(a, b));
    }
  }
