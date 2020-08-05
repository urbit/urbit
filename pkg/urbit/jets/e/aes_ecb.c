/* j/5/aes_ecb.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/

  static u3_atom
  _cqea_ecba_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16], blk_y[16], out_y[16];

    u3r_bytes(0, 16, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecba_en(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecba_en(a, b);
    }
  }

  static u3_atom
  _cqea_ecba_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16], blk_y[16], out_y[16];

    u3r_bytes(0, 16, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecba_de(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecba_de(a, b);
    }
  }

  static u3_atom
  _cqea_ecbb_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24], blk_y[16], out_y[16];

    u3r_bytes(0, 24, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecbb_en(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecbb_en(a, b);
    }
  }

  static u3_atom
  _cqea_ecbb_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24], blk_y[16], out_y[16];

    u3r_bytes(0, 24, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecbb_de(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecbb_de(a, b);
    }
  }

  static u3_atom
  _cqea_ecbc_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32], blk_y[16], out_y[16];

    u3r_bytes(0, 32, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecbc_en(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecbc_en(a, b);
    }
  }

  static u3_atom
  _cqea_ecbc_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32], blk_y[16], out_y[16];

    u3r_bytes(0, 32, key_y, key);
    u3r_bytes(0, 16, blk_y, blk);

    if ( 0 != urcrypt_aes_ecbc_de(key_y, blk_y, out_y) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(16, out_y);
    }
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
      return _cqea_ecbc_de(a, b);
    }
  }
