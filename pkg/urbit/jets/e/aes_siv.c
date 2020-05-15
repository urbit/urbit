/* j/5/aes_ecb.c
**
*/
#include "all.h"

#include <openssl/aes.h>

#include "aes_siv.h"


/* functions
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

static u3_noun _siv_en(c3_y* key_y,
                       c3_w keysize,
                       u3_atom ads,
                       u3_atom txt)
{
  AES_SIV_CTX* ctx = AES_SIV_CTX_new();
  if ( 0 == AES_SIV_Init(ctx, key_y, keysize) ) {
    AES_SIV_CTX_free(ctx);
    return u3m_bail(c3__exit);
  }

  while (u3_nul != ads) {
    c3_w ad_w = u3r_met(3, u3h(ads));
    c3_y* ad_y = u3a_malloc(ad_w);
    u3r_bytes_reverse(0, ad_w, ad_y, u3h(ads));

    c3_w ret = AES_SIV_AssociateData(ctx, ad_y, ad_w);
    u3a_free(ad_y);

    if ( 0 == ret ) {
      AES_SIV_CTX_free(ctx);
      return u3m_bail(c3__exit);
    }

    ads = u3t(ads);
  }

  c3_w txt_w = u3r_met(3, txt);
  c3_y* txt_y = u3a_malloc(txt_w);
  u3r_bytes_reverse(0, txt_w, txt_y, txt);

  c3_w iv_w = 16;
  c3_y* iv_y = u3a_malloc(iv_w);
  c3_w out_w = txt_w;
  c3_y* out_y = u3a_malloc(out_w);
  if ( 0 == AES_SIV_EncryptFinal(ctx, iv_y, out_y, txt_y, txt_w) ) {
    u3a_free(iv_y);
    u3a_free(out_y);
    u3a_free(txt_y);
    AES_SIV_CTX_free(ctx);
    return u3m_bail(c3__exit);
  }

  u3a_free(txt_y);
  AES_SIV_CTX_free(ctx);

  // Read the first 16 bytes as the "iv"
  u3_noun iv_n = u3i_bytes(16, iv_y);
  u3_noun msg_n = u3i_bytes(txt_w, out_y);

  // Reverse byte order for output
  u3_noun rev_iv_n = u3qc_rev(3, iv_w, iv_n);
  u3_noun rev_msg_n = u3qc_rev(3, txt_w, msg_n);

  u3a_free(iv_y);
  u3a_free(out_y);
  u3z(iv_n);
  u3z(msg_n);

  return u3nt(rev_iv_n, txt_w, rev_msg_n);
}

static u3_noun _siv_de(c3_y* key_y,
                       c3_w keysize,
                       u3_atom ads,
                       u3_atom iv,
                       u3_atom len,
                       u3_atom txt)
{
  AES_SIV_CTX* ctx = AES_SIV_CTX_new();
  if ( 0 == AES_SIV_Init(ctx, key_y, keysize) ) {
    AES_SIV_CTX_free(ctx);
    return u3m_bail(c3__exit);
  }

  while (u3_nul != ads) {
    c3_w ad_w = u3r_met(3, u3h(ads));
    c3_y* ad_y = u3a_malloc(ad_w);
    u3r_bytes_reverse(0, ad_w, ad_y, u3h(ads));

    c3_w ret = AES_SIV_AssociateData(ctx, ad_y, ad_w);
    u3a_free(ad_y);

    if ( 0 == ret ) {
      AES_SIV_CTX_free(ctx);
      return u3m_bail(c3__exit);
    }

    ads = u3t(ads);
  }

  c3_w txt_w = u3r_word(0, len);  // ?
  c3_y* txt_y = u3a_malloc(txt_w);
  u3r_bytes_reverse(0, txt_w, txt_y, txt);

  c3_w iv_w = 16;
  c3_y* iv_y = u3a_malloc(iv_w);
  u3r_bytes_reverse(0, 16, iv_y, iv);

  c3_y* out_y = u3a_malloc(txt_w);
  if ( 0 == AES_SIV_DecryptFinal(ctx, out_y, iv_y, txt_y, txt_w) ) {
    u3a_free(iv_y);
    u3a_free(out_y);
    u3a_free(txt_y);
    AES_SIV_CTX_free(ctx);

    // Dcryption failed or signature bad.
    return 0;
  }

  u3a_free(txt_y);
  AES_SIV_CTX_free(ctx);

  // Read the first 16 bytes as the "iv"
  u3_noun msg_n = u3i_bytes(txt_w, out_y);

  // Reverse byte order for output
  u3_noun rev_msg_n = u3qc_rev(3, txt_w, msg_n);

  u3a_free(iv_y);
  u3a_free(out_y);
  u3z(msg_n);

  return u3nc(0, rev_msg_n);
}


u3_noun
u3qea_siva_en(u3_atom key,
              u3_atom ads,
              u3_atom txt)
{
  c3_y key_y[32];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 32);

  u3r_bytes_reverse(0, 32, key_y, key);
  return _siv_en(key_y, 32, ads, txt);
}

u3_noun
u3wea_siva_en(u3_noun cor)
{
  u3_noun key, ads, txt;

  if ( c3n == u3r_mean(cor, u3x_sam, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_siva_en(key, ads, txt);
  }
}

u3_noun
u3qea_siva_de(u3_atom key,
              u3_atom ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[32];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 32);

  u3r_bytes_reverse(0, 32, key_y, key);
  return _siv_de(key_y, 32, ads, iv, len, txt);
}

u3_noun
u3wea_siva_de(u3_noun cor)
{
  u3_noun key, ads, iv, len, txt;

  if ( c3n == u3r_mean(cor,
                       u3x_sam_2, &iv,
                       u3x_sam_6, &len,
                       u3x_sam_7, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_siva_de(key, ads, iv, len, txt);
  }
}


u3_noun
u3qea_sivb_en(u3_atom key,
              u3_atom ads,
              u3_atom txt)
{
  c3_y key_y[48];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 48);

  u3r_bytes_reverse(0, 48, key_y, key);
  return _siv_en(key_y, 48, ads, txt);
}

u3_noun
u3wea_sivb_en(u3_noun cor)
{
  u3_noun key, ads, txt;

  if ( c3n == u3r_mean(cor, u3x_sam, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_sivb_en(key, ads, txt);
  }
}

u3_noun
u3qea_sivb_de(u3_atom key,
              u3_atom ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[48];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 48);

  u3r_bytes_reverse(0, 48, key_y, key);
  return _siv_de(key_y, 48, ads, iv, len, txt);
}

u3_noun
u3wea_sivb_de(u3_noun cor)
{
  u3_noun key, ads, iv, len, txt;

  if ( c3n == u3r_mean(cor,
                       u3x_sam_2, &iv,
                       u3x_sam_6, &len,
                       u3x_sam_7, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_sivb_de(key, ads, iv, len, txt);
  }
}



u3_noun
u3qea_sivc_en(u3_atom key,
              u3_atom ads,
              u3_atom txt)
{
  c3_y key_y[64];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 64);

  u3r_bytes_reverse(0, 64, key_y, key);
  return _siv_en(key_y, 64, ads, txt);
}

u3_noun
u3wea_sivc_en(u3_noun cor)
{
  u3_noun key, ads, txt;

  if ( c3n == u3r_mean(cor, u3x_sam, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_sivc_en(key, ads, txt);
  }
}


u3_noun
u3qea_sivc_de(u3_atom key,
              u3_atom ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[64];
  AES_KEY key_u;
  c3_assert(u3r_met(3, key) <= 64);

  u3r_bytes_reverse(0, 64, key_y, key);
  return _siv_de(key_y, 64, ads, iv, len, txt);
}

u3_noun
u3wea_sivc_de(u3_noun cor)
{
  u3_noun key, ads, iv, len, txt;

  if ( c3n == u3r_mean(cor,
                       u3x_sam_2, &iv,
                       u3x_sam_6, &len,
                       u3x_sam_7, &txt,
                       u3x_con_sam_2, &key,
                       u3x_con_sam_3, &ads, 0) ||
       c3n == u3ud(key) ||
       c3n == u3ud(txt) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qea_sivc_de(key, ads, iv, len, txt);
  }
}
