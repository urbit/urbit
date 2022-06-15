#include "urcrypt.h"
#include "util.h"
#include <string.h>
#include <libec.h>
#include <stdio.h>
#include <stdlib.h>

#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a < _b ? _a : _b; })

typedef struct {
  prj_pt p0;
  prj_pt p1;
  prj_pt p2;
  prj_pt p3;
  prj_pt p4;
} constant_points;

struct urcrypt_pedersen_context_struct {
  constant_points *cp;
  ec_params *curve_params;
};

size_t
urcrypt_pedersen_context_size()
{
  return sizeof(urcrypt_pedersen_context);
}

int
get_curve_params(ec_params *curve_params) {
  int ret;
  printf("getting curve params\r\n");
  u8 curve_name[MAX_CURVE_NAME_LEN] = "USER_DEFINED_STARK";
  u32 len = strnlen((const char *)curve_name, MAX_CURVE_NAME_LEN);
  len += 1;
  const ec_str_params *curve_string_params;
  ret = ec_get_curve_params_by_name(curve_name, (u8)len, &curve_string_params);
  if (curve_string_params == NULL || ret != 0) {
    return -1;
  }
  ret = import_params(curve_params, curve_string_params);
  return ret;
}

int
get_constant_points(ec_params *curve_params, constant_points* cp) {
  printf("getting constant points\r\n");
  int ret = 0;
  ec_shortw_crv_src_t crv = &(curve_params->ec_curve);

  const u8 p0_buf[] = {
    // x coordinate
    0x04, 0x9e, 0xe3, 0xeb, 0xa8, 0xc1, 0x60, 0x07,
    0x00, 0xee, 0x1b, 0x87, 0xeb, 0x59, 0x9f, 0x16,
    0x71, 0x6b, 0x0b, 0x10, 0x22, 0x94, 0x77, 0x33,
    0x55, 0x1f, 0xde, 0x40, 0x50, 0xca, 0x68, 0x04,

    // y coordinate
    0x03, 0xca, 0x0c, 0xfe, 0x4b, 0x3b, 0xc6, 0xdd,
    0xf3, 0x46, 0xd4, 0x9d, 0x06, 0xea, 0x0e, 0xd3,
    0x4e, 0x62, 0x10, 0x62, 0xc0, 0xe0, 0x56, 0xc1,
    0xd0, 0x40, 0x5d, 0x26, 0x6e, 0x10, 0x26, 0x8a
  };

  const u8 p1_buf[] = {
    // x coordinate
    0x02, 0x34, 0x28, 0x7d, 0xcb, 0xaf, 0xfe, 0x7f,
    0x96, 0x9c, 0x74, 0x86, 0x55, 0xfc, 0xa9, 0xe5,
    0x8f, 0xa8, 0x12, 0x0b, 0x6d, 0x56, 0xeb, 0x0c,
    0x10, 0x80, 0xd1, 0x79, 0x57, 0xeb, 0xe4, 0x7b,

    // y coordinate
    0x03, 0xb0, 0x56, 0xf1, 0x00, 0xf9, 0x6f, 0xb2,
    0x1e, 0x88, 0x95, 0x27, 0xd4, 0x1f, 0x4e, 0x39,
    0x94, 0x01, 0x35, 0xdd, 0x7a, 0x6c, 0x94, 0xcc,
    0x6e, 0xd0, 0x26, 0x8e, 0xe8, 0x9e, 0x56, 0x15
  };

  const u8 p2_buf[] = {
    // x coordinate
    0x04, 0xfa, 0x56, 0xf3, 0x76, 0xc8, 0x3d, 0xb3,
    0x3f, 0x9d, 0xab, 0x26, 0x56, 0x55, 0x8f, 0x33,
    0x99, 0x09, 0x9e, 0xc1, 0xde, 0x5e, 0x30, 0x18,
    0xb7, 0xa6, 0x93, 0x2d, 0xba, 0x8a, 0xa3, 0x78,

    // y coordinate
    0x03, 0xfa, 0x09, 0x84, 0xc9, 0x31, 0xc9, 0xe3,
    0x81, 0x13, 0xe0, 0xc0, 0xe4, 0x7e, 0x44, 0x01,
    0x56, 0x27, 0x61, 0xf9, 0x2a, 0x7a, 0x23, 0xb4,
    0x51, 0x68, 0xf4, 0xe8, 0x0f, 0xf5, 0xb5, 0x4d
  };

  const u8 p3_buf[] = {
    // x coordinate
    0x04, 0xba, 0x4c, 0xc1, 0x66, 0xbe, 0x8d, 0xec,
    0x76, 0x49, 0x10, 0xf7, 0x5b, 0x45, 0xf7, 0x4b,
    0x40, 0xc6, 0x90, 0xc7, 0x47, 0x09, 0xe9, 0x0f,
    0x3a, 0xa3, 0x72, 0xf0, 0xbd, 0x2d, 0x69, 0x97,

    // y coordinate
    0x00, 0x40, 0x30, 0x1c, 0xf5, 0xc1, 0x75, 0x1f,
    0x4b, 0x97, 0x1e, 0x46, 0xc4, 0xed, 0xe8, 0x5f,
    0xca, 0xc5, 0xc5, 0x9a, 0x5c, 0xe5, 0xae, 0x7c,
    0x48, 0x15, 0x1f, 0x27, 0xb2, 0x4b, 0x21, 0x9c
  };

  const u8 p4_buf[] = {
    // x coordinate
    0x05, 0x43, 0x02, 0xdc, 0xb0, 0xe6, 0xcc, 0x1c,
    0x6e, 0x44, 0xcc, 0xa8, 0xf6, 0x1a, 0x63, 0xbb,
    0x2c, 0xa6, 0x50, 0x48, 0xd5, 0x3f, 0xb3, 0x25,
    0xd3, 0x6f, 0xf1, 0x2c, 0x49, 0xa5, 0x82, 0x02,

    // y coordinate
    0x01, 0xb7, 0x7b, 0x3e, 0x37, 0xd1, 0x35, 0x04,
    0xb3, 0x48, 0x04, 0x62, 0x68, 0xd8, 0xae, 0x25,
    0xce, 0x98, 0xad, 0x78, 0x3c, 0x25, 0x56, 0x1a,
    0x87, 0x9d, 0xcc, 0x77, 0xe9, 0x9c, 0x24, 0x26
  };

  ret = prj_pt_import_from_aff_buf(&(cp->p0), p0_buf, 64, crv);
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_import_from_aff_buf(&(cp->p1), p1_buf, 64, crv);
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_import_from_aff_buf(&(cp->p2), p2_buf, 64, crv);
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_import_from_aff_buf(&(cp->p3), p3_buf, 64, crv);
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_import_from_aff_buf(&(cp->p4), p4_buf, 64, crv);
  return ret;
}

int
do_hash(urcrypt_pedersen_context *cxt, uint8_t *a, size_t a_len, uint8_t *b, size_t b_len, uint8_t out[32])
{
  int ret = 0;

  urcrypt__reverse(a_len, a);
  urcrypt__reverse(b_len, b);

  nn alow, ahig, blow, bhig;
  if (a_len == 32) {
    ret = nn_init_from_buf(&alow, a+1, 31);
    if (ret != 0) {
      return ret;
    }
    ret = nn_init_from_buf(&ahig, a, 1);
    if (ret != 0) {
      return ret;
    }
  } else {
    ret = nn_init(&ahig, 1);
    if (ret != 0) {
      return ret;
    }
    ret = nn_zero(&ahig);
    if (ret != 0) {
      return ret;
    }

    ret = nn_init_from_buf(&alow, a, a_len);
    if (ret != 0) {
      return ret;
    }
  }

  if (b_len == 32) {
    ret = nn_init_from_buf(&blow, b+1, 31);
    if (ret != 0) {
      return ret;
    }
    *b = *b & 0x0f;
    ret = nn_init_from_buf(&bhig, b, 1);
    if (ret != 0) {
      return ret;
    }
  } else {
    ret = nn_init(&bhig, 1);
    if (ret != 0) {
      return ret;
    }
    ret = nn_zero(&bhig);
    if (ret != 0) {
      return ret;
    }

    ret = nn_init_from_buf(&blow, b, b_len);
    if (ret != 0) {
      return ret;
    }
  }

  prj_pt p1_alow, p2_ahig, p3_blow, p4_bhig;
  ret = prj_pt_mul(&p1_alow, &alow, &(cxt->cp->p1));
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_mul(&p2_ahig, &ahig, &(cxt->cp->p2));
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_mul(&p3_blow, &blow, &(cxt->cp->p3));
  if (ret != 0) {
    return ret;
  }

  ret = prj_pt_mul(&p4_bhig, &bhig, &(cxt->cp->p4));
  if (ret != 0) {
    return ret;
  }

  prj_pt sum;
  ret = prj_pt_add(&sum, &(cxt->cp->p0), &p1_alow);
  if (ret != 0) {
    return ret;
  }
  ret = prj_pt_add(&sum, &sum, &p2_ahig);
  if (ret != 0) {
    return ret;
  }
  ret = prj_pt_add(&sum, &sum, &p3_blow);
  if (ret != 0) {
    return ret;
  }
  ret = prj_pt_add(&sum, &sum, &p4_bhig);
  if (ret != 0) {
    return ret;
  }

  prj_pt aff;
  ret = prj_pt_unique(&aff, &sum);
  if (ret != 0) {
    return ret;
  }

  ret = fp_export_to_buf(out, 32, &(aff.X));
  urcrypt__reverse(32, out);
  return ret;
}

static int
do_fold_hash(urcrypt_pedersen_context *cxt, uint8_t *dat_x, size_t len_x, uint8_t *out) {
  int ret = 0;
  int pos = 0, ext_pos = 0, ext_len = (len_x / 32);
  uint8_t *hed = NULL, *acc = NULL, *ext = NULL;
  while (pos < len_x) {
    hed = (dat_x + pos);
    if ( (len_x - pos) >= 32 && (hed[31] & 0xf0) != 0) {
        if (ext == NULL) {
          ext = calloc(1, ext_len);
        }
        ext[ext_pos] = (hed[31] & 0xf0) >> 4;
        ext_pos++;
        hed[31] = hed[31] & 0x0f;
      }
    if (pos == 0) {
      acc = hed;
    } else {
      ret = do_hash(cxt, acc, 32, hed, MIN(len_x - pos, 32), out);
      if (ret != 0) {
        if (ext != NULL) {
          free(ext);
        }
        return ret;
      }
      acc = out;
    }
    pos += 32;
  }
  if (ext != NULL) {
    ret = do_hash(cxt, acc, 32, ext, ext_len, out);
    free(ext);
  }
  return ret;
}

int
urcrypt_pedersen(urcrypt_pedersen_context *cxt, uint8_t *a, size_t a_len, uint8_t *b, size_t b_len, uint8_t out_y[32])
{
  int ret = 0;
  uint8_t a_vat[32], b_vat[32], *a_dat, *b_dat;
  size_t a_ven, b_ven;
  if ( (a_len < 32) ||
       ((a_len == 32) && ((a[31] & 0xf0) == 0)) )
    {
      // a <= 252 bits so use directly
      a_ven = a_len;
      a_dat = a;
    }
  else {
    // a > 252 bits so break up into chunks and fold hash across
    a_ven = 32;
    ret = do_fold_hash(cxt, a, a_len, a_vat);
    if (ret != 0) {
      return ret;
    }
    a_dat = a_vat;
  }

  if ( (b_len < 32) ||
       ((b_len == 32) && ((b[31] & 0xf0) == 0)) )
    {
      // b <= 252 bits
      b_ven = b_len;
      b_dat = b;
    }
  else {
    // b > 252 bits so break up into chunks and fold hash across
    b_ven = 32;
    ret = do_fold_hash(cxt, b, b_len, b_vat);
    if (ret != 0) {
      return ret;
    }
    b_dat = b_vat;
  }

  return do_hash(cxt, a_dat, a_ven, b_dat, b_ven, out_y);
}

int
urcrypt_pedersen_init(urcrypt_pedersen_context *context)
{
  int ret;
  context->cp = malloc(sizeof(constant_points));
  context->curve_params = malloc(sizeof(ec_params));
  ret = get_curve_params(context->curve_params);
  if (ret != 0) {
    return -1;
  }
  return get_constant_points(context->curve_params, context->cp);
}

void
urcrypt_pedersen_destroy(urcrypt_pedersen_context *context)
{
  free(context->cp);
  free(context->curve_params);
}
