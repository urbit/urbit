/* j/5/aes_ecb.c
**
*/
#include "all.h"
#include <urcrypt.h>

#include "aes_siv.h"

typedef int (*urcrypt_siv)(c3_y*, size_t,
                           urcrypt_aes_siv_data*, size_t,
                           c3_y*, c3_y[16], c3_y*);

/* functions
 */
  // soc_w = number of items
  // mat_w = size in bytes of assoc array
  // dat_w = size of allocation (array + atom storage)
  static void
  _cqea_measure_ads(u3_noun ads, c3_w* soc_w, c3_w *mat_w, c3_w *dat_w)
  {
    u3_noun i, t;
    c3_w a_w, b_w, tmp_w, met_w;

    for ( a_w = b_w = 0, t = ads; u3_nul != t; ++a_w ) {
      u3x_cell(t, &i, &t);
      if ( c3n == u3ud(i) ) {
        u3m_bail(c3__exit);
        return;
      }
      else {
        tmp_w = b_w;
        b_w += u3r_met(3, i);
        if ( b_w < tmp_w ) {
          u3m_bail(c3__fail);
          return;
        }
      }
    }

    // check for size overflows
    tmp_w = a_w * sizeof(urcrypt_aes_siv_data);
    if ( (tmp_w / a_w) != sizeof(urcrypt_aes_siv_data) ) {
      u3m_bail(c3__fail);
    }
    else if ( (*dat_w = tmp_w + b_w) < tmp_w ) {
      u3m_bail(c3__fail);
    }
    else {
      *soc_w = a_w;
      *mat_w = tmp_w;
    }
  }

  // assumes ads is a valid (list @) because it's already been measured
  static void
  _cqea_encode_ads(u3_noun ads,
                   c3_w mat_w,
                   urcrypt_aes_siv_data *dat_u)
  {
    c3_w met_w;
    u3_noun i, t;
    urcrypt_aes_siv_data *cur_u;
    c3_y *dat_y = ((c3_y*) dat_u) + mat_w;

    for ( cur_u = dat_u, t = ads; u3_nul != t; t = u3t(t), ++cur_u ) {
      i = u3h(t);
      met_w = u3r_met(3, i);
      u3r_bytes(0, met_w, dat_y, i);
      cur_u->length = met_w;
      cur_u->bytes = dat_y;
      dat_y += met_w;
    }
  }

  static u3_noun
  _cqea_siv_en(c3_y*   key_y,
               c3_w    key_w,
               u3_noun ads,
               u3_atom txt,
               urcrypt_siv low_f)
  {
    u3_noun ret;
    c3_w txt_w, soc_w;
    c3_y *txt_y, *out_y, iv_y[16];
    c3_t ads_t = ( u3_nul != ads );
    urcrypt_aes_siv_data *dat_u;

    if ( !ads_t ) {
      soc_w = 0;
      dat_u = NULL;
    }
    else {
      c3_w mat_w, dat_w;

      _cqea_measure_ads(ads, &soc_w, &mat_w, &dat_w);
      dat_u = u3a_malloc(dat_w);
      _cqea_encode_ads(ads, mat_w, dat_u);
    }

    txt_y = u3r_bytes_all(&txt_w, txt);
    out_y = u3a_malloc(txt_w);

    ret = ( 0 != (*low_f)(txt_y, txt_w, dat_u, soc_w, key_y, iv_y, out_y) )
        ? u3_none
        : u3nt(u3i_bytes(16, iv_y),
               u3i_words(1, &txt_w),
               u3i_bytes(txt_w, out_y));

    u3a_free(txt_y);
    u3a_free(out_y);
    if ( ads_t ) {
      u3a_free(dat_u);
    }

    return ret;
  }

  static u3_noun
  _cqea_siva_en(u3_atom key,
                u3_noun ads,
                u3_atom txt)
  {
    if ( u3r_met(3, key) > 32 ) {
      // hoon doesn't explicitly check size, but we need 32.
      return u3_none;
    }
    else {
      c3_y key_y[32];
      u3r_bytes(0, 32, key_y, key);
      return _cqea_siv_en(key_y, 32, ads, txt, &urcrypt_aes_siva_en);
    }
  }

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
                       u3_noun ads,
                       u3_atom txt)
{
  AES_SIV_CTX* ctx = AES_SIV_CTX_new();
  if ( 0 == ctx ) {
    return u3_none;
  }

  if ( 0 == AES_SIV_Init(ctx, key_y, keysize) ) {
    AES_SIV_CTX_free(ctx);
    return u3_none;
  }

  while (u3_nul != ads) {
    c3_w ad_w = u3r_met(3, u3h(ads));
    c3_y* ad_y = u3a_malloc(ad_w);
    u3r_bytes_reverse(0, ad_w, ad_y, u3h(ads));

    c3_w ret = AES_SIV_AssociateData(ctx, ad_y, ad_w);
    u3a_free(ad_y);

    if ( 0 == ret ) {
      AES_SIV_CTX_free(ctx);
      return u3_none;
    }

    ads = u3t(ads);
  }

  c3_w txt_w = u3r_met(3, txt);
  c3_y* txt_y = u3a_malloc(txt_w);
  u3r_bytes_reverse(0, txt_w, txt_y, txt);

  const c3_w iv_w = 16;
  c3_y iv_y[iv_w];
  c3_y* out_y = u3a_malloc(txt_w);
  if ( 0 == AES_SIV_EncryptFinal(ctx, iv_y, out_y, txt_y, txt_w) ) {
    u3a_free(out_y);
    u3a_free(txt_y);
    AES_SIV_CTX_free(ctx);
    return u3_none;
  }

  u3a_free(txt_y);
  AES_SIV_CTX_free(ctx);

  // Read the first 16 bytes as the "iv"
  u3_noun iv = u3i_bytes(16, iv_y);
  u3_noun msg = u3i_bytes(txt_w, out_y);

  // Reverse byte order for output
  u3_noun rev_iv = u3kc_rev(3, iv_w, iv);
  u3_noun rev_msg = u3kc_rev(3, txt_w, msg);

  u3a_free(out_y);

  return u3nt(rev_iv, u3i_words(1, &txt_w), rev_msg);
}

static u3_noun _siv_de(c3_y* key_y,
                       c3_w keysize,
                       u3_noun ads,
                       u3_atom iv,
                       u3_atom len,
                       u3_atom txt)
{
  AES_SIV_CTX* ctx = AES_SIV_CTX_new();
  if ( 0 == ctx ) {
    return u3_none;
  }

  if ( 0 == AES_SIV_Init(ctx, key_y, keysize) ) {
    AES_SIV_CTX_free(ctx);
    return u3_none;
  }

  while (u3_nul != ads) {
    c3_w ad_w = u3r_met(3, u3h(ads));
    c3_y* ad_y = u3a_malloc(ad_w);
    u3r_bytes_reverse(0, ad_w, ad_y, u3h(ads));

    c3_w ret = AES_SIV_AssociateData(ctx, ad_y, ad_w);
    u3a_free(ad_y);

    if ( 0 == ret ) {
      AES_SIV_CTX_free(ctx);
      return u3_none;
    }

    ads = u3t(ads);
  }

  c3_w txt_w = u3r_word(0, len);
  c3_y* txt_y = u3a_malloc(txt_w);
  u3r_bytes_reverse(0, txt_w, txt_y, txt);

  const c3_w iv_w = 16;
  c3_y iv_y[iv_w];
  u3r_bytes_reverse(0, 16, iv_y, iv);

  c3_y* out_y = u3a_malloc(txt_w);
  if ( 0 == AES_SIV_DecryptFinal(ctx, out_y, iv_y, txt_y, txt_w) ) {
    u3a_free(out_y);
    u3a_free(txt_y);
    AES_SIV_CTX_free(ctx);

    // Either decryption failed or signature bad or there was a memory
    // error. Some of these are deterministic and some are not. return u3_none
    // to fallback to the Nock implementation.
    return u3_none;
  }

  u3a_free(txt_y);
  AES_SIV_CTX_free(ctx);

  // Read the first 16 bytes as the "iv"
  u3_noun msg = u3i_bytes(txt_w, out_y);

  // Reverse byte order for output
  u3_noun rev_msg = u3kc_rev(3, txt_w, msg);

  u3a_free(out_y);

  return u3nc(0, rev_msg);
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
    return _cqea_siva_en(key, ads, txt);
  }
}

u3_noun
u3qea_siva_de(u3_atom key,
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[32];
  if (u3r_met(3, key) > 32) {
    return u3_none;
  }

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
              u3_noun ads,
              u3_atom txt)
{
  c3_y key_y[48];
  if (u3r_met(3, key) > 48) {
    return u3_none;
  }

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
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[48];
  if (u3r_met(3, key) > 48) {
    return u3_none;
  }

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
              u3_noun ads,
              u3_atom txt)
{
  c3_y key_y[64];
  if (u3r_met(3, key) > 64) {
    return u3_none;
  }

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
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  c3_y key_y[64];
  if (u3r_met(3, key) > 64) {
    return u3_none;
  }

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
