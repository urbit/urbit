
/* j/5/pedersen.c
**
*/
#include "all.h"
#include <urcrypt.h>

static c3_y*
u3r_bytes_all_works(c3_w* len_w, u3_atom a)
{
  c3_w met_w;
  if (a == u3_nul) {
    met_w = *len_w = 1;
  } else {
    met_w = *len_w = u3r_met(3, a);
  }
  return u3r_bytes_alloc(0, met_w, a);
}

static c3_y *
fold_hash(u3_atom x, c3_w *len_acc) {
  c3_w len_x, pos = 0, ext_pos = 0;
  c3_y *acc = NULL, *out = NULL, *ext = NULL;
  c3_y *hed, *dat_x = u3r_bytes_all_works(&len_x, x);
  c3_w ext_len = (len_x / 64) + 1;
  bool pol = true; // polarity - true=byte aligned, false=shift 4 bits
  while (1) {
    hed = (dat_x + pos);
    if (pol && (len_x - pos) >= 32) {
      // if positive polarity and chunk is the full 252 bits then
      // zero out the final 4 bits but save them in *ext to hash
      // at the very end
      if ((hed[31] & 0xf0) != 0) {
        if (ext == NULL) {
          ext = u3a_malloc(ext_len);
        }
        ext[ext_pos] = (hed[31] & 0xf0) >> 4;
        ext_pos++;
        hed[31] = hed[31] & 0x0f;
      }
    }
    if (pos == 0) {
      acc = hed;
      *len_acc = c3_min(len_x, 32);
    } else {
      if (out == NULL) {
        out = u3a_malloc(32);
      }
      urcrypt_pedersen(acc, 32, hed, len_x - pos, out);
      acc = out;
    }
    if ((len_x - pos) > 32) {
      pos += 32;
    } else {
      break;
    }
    pol = !pol;
  }
  if (ext != NULL) {
    urcrypt_pedersen(acc, 32, ext, ext_len, out);
    acc = out;
    u3a_free(ext);
  }
  if (len_x > 32) {
    u3a_free(dat_x);
  }
  return acc;
}

static u3_noun
_cqe_phash(u3_atom x, u3_atom y)
{
  c3_w len_x, len_y;
  c3_y *dat_x = fold_hash(x, &len_x);
  c3_y *dat_y = fold_hash(y, &len_y);
  c3_y out_y[32];
  urcrypt_pedersen(dat_x, len_x, dat_y, len_y, out_y);
  u3a_free(dat_x);
  u3a_free(dat_y);

  return u3i_bytes(32, out_y);
}

u3_noun
u3we_phash(u3_noun cor)
{
    u3_noun x, y;
    if ( (c3n == u3r_mean(cor,
                          u3x_sam_2,  &x,
                          u3x_sam_3,  &y,
                          0)) ||
         (c3n == u3ud(x)) ||
         (c3n == u3ud(y)) )
    {
        return u3m_bail(c3__exit);
    }
    else {
        return _cqe_phash(x, y);
    }
}
