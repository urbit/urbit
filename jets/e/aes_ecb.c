/* j/5/aes_ecb.c
**
*/
#include "all.h"

#include <openssl/aes.h>

/* functions
*/
  u3_noun
  u3qea_ecba_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 16);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 15;

      do {
        key_y[i] = u3r_byte(15-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 128, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    return u3i_bytes(16, blk_y);
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
      return u3qea_ecba_en(a, b);
    }
  }

  u3_noun
  u3qea_ecba_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[16];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 16);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 15;

      do {
        key_y[i] = u3r_byte(15-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 128, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }
    return u3i_bytes(16, blk_y);
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
      return u3qea_ecba_de(a, b);
    }
  }

  u3_noun
  u3qea_ecbb_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 24);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 23;

      do {
        key_y[i] = u3r_byte(23-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 192, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    return u3i_bytes(16, blk_y);
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
      return u3qea_ecbb_en(a, b);
    }
  }

  u3_noun
  u3qea_ecbb_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[24];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 24);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 23;

      do {
        key_y[i] = u3r_byte(23-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 192, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }
    return u3i_bytes(16, blk_y);
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
      return u3qea_ecbb_de(a, b);
    }
  }

  u3_noun
  u3qea_ecbc_en(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 32);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 31;

      do {
        key_y[i] = u3r_byte(31-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    return u3i_bytes(16, blk_y);
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
      return u3qea_ecbc_en(a, b);
    }
  }

  u3_noun
  u3qea_ecbc_de(u3_atom key,
                u3_atom blk)
  {
    c3_y key_y[32];
    c3_y blk_y[16];
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 32);
    c3_assert(u3r_met(3, blk) <= 16);

    {
      int i = 31;

      do {
        key_y[i] = u3r_byte(31-i, key);
        i--;
      } while (i >= 0);
    }
    {
      int i = 15;

      do {
        blk_y[i] = u3r_byte(15-i, blk);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_ecb_encrypt(blk_y, blk_y, &key_u, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = 15;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = blk_y[i];
        blk_y[i] = blk_y[j];
        blk_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }
    return u3i_bytes(16, blk_y);
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
      return u3qea_ecbc_de(a, b);
    }
  }
