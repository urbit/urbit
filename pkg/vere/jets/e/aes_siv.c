/* j/5/aes_ecb.c
**
*/
#include "all.h"
#include <urcrypt.h>

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

static void
_cqea_ads_free(urcrypt_aes_siv_data *dat_u)
{
  if ( NULL != dat_u ) {
    u3a_free(dat_u);
  }
}

static urcrypt_aes_siv_data*
_cqea_ads_alloc(u3_noun ads, c3_w *soc_w)
{
  if ( !ads ) {
    *soc_w = 0;
    return NULL;
  }
  else {
    c3_w mat_w, dat_w;
    urcrypt_aes_siv_data *dat_u;

    _cqea_measure_ads(ads, soc_w, &mat_w, &dat_w);
    dat_u = u3a_malloc(dat_w);
    _cqea_encode_ads(ads, mat_w, dat_u);
    return dat_u;
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
  urcrypt_aes_siv_data *dat_u;

  dat_u = _cqea_ads_alloc(ads, &soc_w);
  txt_y = u3r_bytes_all(&txt_w, txt);
  out_y = u3a_malloc(txt_w);

  ret = ( 0 != (*low_f)(txt_y, txt_w, dat_u, soc_w, key_y, iv_y, out_y) )
      ? u3_none
      : u3nt(u3i_bytes(16, iv_y),
             u3i_words(1, &txt_w),
             u3i_bytes(txt_w, out_y));

  u3a_free(txt_y);
  u3a_free(out_y);
  _cqea_ads_free(dat_u);
  return ret;
}

static u3_noun
_cqea_siv_de(c3_y*   key_y,
             c3_w    key_w,
             u3_noun ads,
             u3_atom iv,
             u3_atom len,
             u3_atom txt,
             urcrypt_siv low_f)
{
  c3_w txt_w;
  if ( !u3r_word_fit(&txt_w, len) ) {
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun ret;
    c3_w soc_w;
    c3_y *txt_y, *out_y, iv_y[16];
    urcrypt_aes_siv_data *dat_u;

    u3r_bytes(0, 16, iv_y, iv);
    dat_u = _cqea_ads_alloc(ads, &soc_w);
    txt_y = u3r_bytes_alloc(0, txt_w, txt);
    out_y = u3a_malloc(txt_w);

    ret = ( 0 != (*low_f)(txt_y, txt_w, dat_u, soc_w, key_y, iv_y, out_y) )
      ? u3_none
      : u3nc(0, u3i_bytes(txt_w, out_y));

    u3a_free(txt_y);
    u3a_free(out_y);
    _cqea_ads_free(dat_u);

    return ret;
  }
}

// the siv* hoon doesn't explicitly check keysizes, but all of these functions
// have fixed maximum keysizes, so we will punt if we get a key that is too
// large.

static u3_noun
_cqea_siva_en(u3_atom key,
              u3_noun ads,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 32 ) {
    return u3_none;
  }
  else {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_siv_en(key_y, 32, ads, txt, &urcrypt_aes_siva_en);
  }
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
    return u3l_punt("siva-en", _cqea_siva_en(key, ads, txt));
  }
}

static u3_noun
_cqea_siva_de(u3_atom key,
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 32 ) {
    return u3_none;
  }
  else {
    c3_y key_y[32];
    u3r_bytes(0, 32, key_y, key);
    return _cqea_siv_de(key_y, 32, ads, iv, len, txt, &urcrypt_aes_siva_de);
  }
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
    return u3l_punt("siva-de", _cqea_siva_de(key, ads, iv, len, txt));
  }
}

static u3_noun
_cqea_sivb_en(u3_atom key,
              u3_noun ads,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 48 ) {
    return u3_none;
  }
  else {
    c3_y key_y[48];
    u3r_bytes(0, 48, key_y, key);
    return _cqea_siv_en(key_y, 48, ads, txt, &urcrypt_aes_sivb_en);
  }
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
    return u3l_punt("sivb-en", _cqea_sivb_en(key, ads, txt));
  }
}

static u3_noun
_cqea_sivb_de(u3_atom key,
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 48 ) {
    return u3_none;
  }
  else {
    c3_y key_y[48];
    u3r_bytes(0, 48, key_y, key);
    return _cqea_siv_de(key_y, 48, ads, iv, len, txt, &urcrypt_aes_sivb_de);
  }
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
    return u3l_punt("sivb-de", _cqea_sivb_de(key, ads, iv, len, txt));
  }
}

static u3_noun
_cqea_sivc_en(u3_atom key,
              u3_noun ads,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 64 ) {
    return u3_none;
  }
  else {
    c3_y key_y[64];
    u3r_bytes(0, 64, key_y, key);
    return _cqea_siv_en(key_y, 64, ads, txt, &urcrypt_aes_sivc_en);
  }
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
    return u3l_punt("sivc-en", _cqea_sivc_en(key, ads, txt));
  }
}

static u3_noun
_cqea_sivc_de(u3_atom key,
              u3_noun ads,
              u3_atom iv,
              u3_atom len,
              u3_atom txt)
{
  if ( u3r_met(3, key) > 64 ) {
    return u3_none;
  }
  else {
    c3_y key_y[64];
    u3r_bytes(0, 64, key_y, key);
    return _cqea_siv_de(key_y, 64, ads, iv, len, txt, &urcrypt_aes_sivc_de);
  }
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
    return u3l_punt("sivc-de", _cqea_sivc_de(key, ads, iv, len, txt));
  }
}
