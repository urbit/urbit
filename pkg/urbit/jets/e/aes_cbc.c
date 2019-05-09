/* j/5/aes_cbc.c
**
*/
#include "all.h"

#include <openssl/aes.h>

/* functions
*/
  u3_noun
  u3qea_cbca_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[16];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 16);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 128, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbca_en(a, b, c);
    }
  }

  u3_noun
  u3qea_cbca_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[16];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 16);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 128, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbca_de(a, b, c);
    }
  }

  u3_noun
  u3qea_cbcb_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[24];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 24);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 192, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbcb_en(a, b, c);
    }
  }

  u3_noun
  u3qea_cbcb_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[24];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 24);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 192, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbcb_de(a, b, c);
    }
  }

  u3_noun
  u3qea_cbcc_en(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[32];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 32);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_encrypt_key(key_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_ENCRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbcc_en(a, b, c);
    }
  }

  u3_noun
  u3qea_cbcc_de(u3_atom key,
                u3_atom iv,
                u3_atom msg)
  {
    c3_y key_y[32];
    c3_y iv_y[16];
    c3_w len_msg_w;
    c3_w len_out_w;
    c3_y *msg_y;
    c3_y *out_y;
    u3_atom out;
    AES_KEY key_u;

    c3_assert(u3r_met(3, key) <= 32);
    c3_assert(u3r_met(3, iv) <= 16);
    len_msg_w = u3r_met(3, msg);
    len_out_w = (len_msg_w % 16) == 0 ? len_msg_w : len_msg_w + (16 - (len_msg_w % 16));
    len_msg_w = len_out_w;

    msg_y = u3a_malloc(len_msg_w);
    out_y = u3a_malloc(len_out_w);

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
        iv_y[i] = u3r_byte(15-i, iv);
        i--;
      } while (i >= 0);
    }
    {
      int i = len_msg_w - 1;

      do {
        msg_y[i] = u3r_byte((len_msg_w - 1)-i, msg);
        i--;
      } while (i >= 0);
    }

    if ( 0 != AES_set_decrypt_key(key_y, 256, &key_u) ) {
      return u3m_bail(c3__exit);
    }
    else {
      AES_cbc_encrypt(msg_y, out_y, len_msg_w, &key_u, iv_y, AES_DECRYPT);
    }

    /*  array reverse - we can write backwards u3i_bytes   *
     *  in the unlikely event that this becomes a problem  */
    {
      int i = len_out_w - 1;
      int j = 0;
      c3_y tmp;

      do {
        tmp      = out_y[i];
        out_y[i] = out_y[j];
        out_y[j] = tmp;
        i--; j++;
      } while (i > j);
    }

    out = u3i_bytes(len_out_w, out_y);
    u3a_free(msg_y);
    u3a_free(out_y);
    return out;
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
      return u3qea_cbcc_de(a, b, c);
    }
  }
