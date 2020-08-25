#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ur/defs.h"
#include "ur/bitstream.h"

ur_bool_t
ur_bsr_sane(ur_bsr_t *bsr)
{
  ur_bool_t ret = 8 > bsr->off;

  if ( !bsr->left ) {
    ret = ret && (!bsr->off && !bsr->bytes);
  }

  return ret;
}

ur_cue_res_e
ur_bsr_bit(ur_bsr_t *bsr, uint8_t *out)
{
  uint64_t left = bsr->left;

  if ( !left ) {
    return ur_cue_gone;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint8_t      bit = (b[0] >> off) & 1;

    if ( 7 == off ) {
      bsr->bytes = ( --left ) ? (b + 1) : 0;
      bsr->left  = left;
      bsr->off   = 0;
    }
    else {
      bsr->off   = 1 + off;
    }

    bsr->bits++;

    *out = bit;
    return ur_cue_good;
  }
}

uint8_t
ur_bsr_bit_any(ur_bsr_t *bsr)
{
  uint64_t left = bsr->left;

  bsr->bits++;

  if ( !left ) {
    return 0;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint8_t      bit = (b[0] >> off) & 1;

    if ( 7 == off ) {
      bsr->bytes = ( --left ) ? (b + 1) : 0;
      bsr->left  = left;
      bsr->off   = 0;
    }
    else {
      bsr->off   = 1 + off;
    }

    return bit;
  }
}

uint8_t
ur_bsr8_any(ur_bsr_t *bsr, uint8_t len)
{
  uint64_t left = bsr->left;

  len = ur_min(8, len);

  bsr->bits += len;

  if ( !left ) {
    return 0;
  }
  else {
    uint8_t      off = bsr->off;
    uint8_t     rest = 8 - off;
    const uint8_t *b = bsr->bytes;
    uint8_t        m = b[0] >> off;

    if ( len < rest ) {
      bsr->off = off + len;
      return m & ((1 << len) - 1);
    }
    else if ( 1 == left ) {
      bsr->off   = 0;
      bsr->left  = 0;
      bsr->bytes = 0;
      return m;
    }
    else {
      off = len - rest;

      bsr->off = off;
      bsr->left--;
      bsr->bytes++;

      {
        uint8_t l = b[1] & ((1 << off) - 1);
        return m ^ (l << rest);
      }
    }
  }
}

uint32_t
ur_bsr32_any(ur_bsr_t *bsr, uint8_t len)
{
  uint64_t left = bsr->left;

  len = ur_min(32, len);

  bsr->bits += len;

  if ( !left ) {
    return 0;
  }
  else {
    uint8_t      off = bsr->off;
    uint8_t     rest = 8 - off;
    const uint8_t *b = bsr->bytes;
    uint32_t       m = b[0] >> off;

    if ( len < rest ) {
      bsr->off = off + len;
      return m & ((1 << len) - 1);
    }
    else {
      uint8_t mask, len_byt;
      uint32_t   l;

      len -= rest;
      left--;
      bsr->bytes++;

      len_byt = len >> 3;

      if ( len_byt >= left ) {
        len_byt    = left;
        bsr->off   = off = 0;
        bsr->left  = 0;
        bsr->bytes = 0;
      }
      else {
        bsr->off    = off = ur_mask_3(len);
        bsr->left   = left - len_byt;
        bsr->bytes += len_byt;
      }

      mask = (1 << off) - 1;

      switch ( len_byt ) {
        case 4: {
          l = (uint32_t)b[1]
            ^ (uint32_t)b[2] << 8
            ^ (uint32_t)b[3] << 16
            ^ (uint32_t)b[4] << 24;
        } break;

        case 3: {
          l = (uint32_t)b[1]
            ^ (uint32_t)b[2] << 8
            ^ (uint32_t)b[3] << 16
            ^ (uint32_t)(b[4] & mask) << 24;
        } break;

        case 2: {
          l = (uint32_t)b[1]
            ^ (uint32_t)b[2] << 8
            ^ (uint32_t)(b[3] & mask) << 16;
        } break;

        case 1: {
          l = (uint32_t)b[1]
            ^ (uint32_t)(b[2] & mask) << 8;
        } break;

        case 0: {
           l = (uint32_t)(b[1] & mask);
        } break;
      }

      return m ^ (l << rest);
    }
  }
}

uint64_t
ur_bsr64_any(ur_bsr_t *bsr, uint8_t len)
{
  uint64_t left = bsr->left;

  len = ur_min(64, len);

  bsr->bits += len;

  if ( !left ) {
    return 0;
  }
  else {
    uint8_t      off = bsr->off;
    uint8_t     rest = 8 - off;
    const uint8_t *b = bsr->bytes;
    uint64_t       m = b[0] >> off;

    if ( len < rest ) {
      bsr->off = off + len;
      return m & ((1 << len) - 1);
    }
    else {
      uint8_t mask, len_byt;
      uint64_t   l;

      len -= rest;
      left--;
      bsr->bytes++;

      len_byt = len >> 3;

      if ( len_byt >= left ) {
        len_byt    = left;
        bsr->off   = off = 0;
        bsr->left  = 0;
        bsr->bytes = 0;
      }
      else {
        bsr->off    = off = ur_mask_3(len);
        bsr->left   = left - len_byt;
        bsr->bytes += len_byt;
      }

      mask = (1 << off) - 1;

      switch ( len_byt ) {
        case 8: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)b[4] << 24
            ^ (uint64_t)b[5] << 32
            ^ (uint64_t)b[6] << 40
            ^ (uint64_t)b[7] << 48
            ^ (uint64_t)b[8] << 56;
        } break;

        case 7: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)b[4] << 24
            ^ (uint64_t)b[5] << 32
            ^ (uint64_t)b[6] << 40
            ^ (uint64_t)b[7] << 48
            ^ (uint64_t)(b[8] & mask) << 56;
        } break;

        case 6: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)b[4] << 24
            ^ (uint64_t)b[5] << 32
            ^ (uint64_t)b[6] << 40
            ^ (uint64_t)(b[7] & mask) << 48;
        } break;

        case 5: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)b[4] << 24
            ^ (uint64_t)b[5] << 32
            ^ (uint64_t)(b[6] & mask) << 40;
        } break;

        case 4: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)b[4] << 24
            ^ (uint64_t)(b[5] & mask) << 32;
        } break;

        case 3: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)b[3] << 16
            ^ (uint64_t)(b[4] & mask) << 24;
        } break;

        case 2: {
          l = (uint64_t)b[1]
            ^ (uint64_t)b[2] << 8
            ^ (uint64_t)(b[3] & mask) << 16;
        } break;

        case 1: {
          l = (uint64_t)b[1]
            ^ (uint64_t)(b[2] & mask) << 8;
        } break;

        case 0: {
           l = (uint64_t)(b[1] & mask);
        } break;
      }

      return m ^ (l << rest);
    }
  }
}

void
ur_bsr_bytes_any(ur_bsr_t *bsr, uint64_t len, uint8_t *out)
{
  uint64_t left = bsr->left;

  if ( !left ) {
    return;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint64_t len_byt = len >> 3;
    uint8_t  len_bit = ur_mask_3(len);

    if ( !off ) {
      uint8_t  bits = off + len_bit;
      uint64_t need = len_byt + (bits >> 3) + !!ur_mask_3(bits);

      if ( need > left ) {
        memcpy(out, b, left);
        bsr->bytes = 0;
        bsr->left = 0;
      }
      else {
        memcpy(out, b, len_byt);
        off   = len_bit;
        left -= len_byt;

        if ( !left ) {
          bsr->bytes = 0;
        }
        else {
          bsr->bytes += len_byt;
        }

        bsr->left = left;

        if ( off ) {
          out[len_byt] = b[len_byt] & ((1 << off) - 1);
        }
      }
    }
    //  the most-significant bits from a byte in the stream
    //  become the least-significant bits of an output byte, and vice-versa
    //
    else {
      uint64_t  need = len_byt + (len_bit >> 3) + !!ur_mask_3(len_bit);
      ur_bool_t  end = need >= left;
      uint64_t   max = end ? (left - 1) : len_byt;
      uint8_t   rest = 8 - off;
      uint8_t   mask = (1 << off) - 1;
      uint8_t    byt = b[0];
      uint8_t   l, m = byt >> off;
      uint64_t     i;

      for ( i = 0; i < max; i++ ) {
        byt    = b[1ULL + i];
        l      = byt & mask;
        out[i] = m ^ (l << rest);
        m      = byt >> off;
      }

      if ( end ) {
        if ( len_bit && len_bit < rest ) {
          out[max] = m & ((1 << len_bit) - 1);
          bsr->bytes += max;
          left -= max;
          off += len_bit;
        }
        else {
          out[max] = m;
          bsr->bytes = 0;
          left = 0;
          off  = 0;
        }
      }
      else {
        uint8_t  bits = off + len_bit;
        uint64_t step = max + !!(bits >> 3);

        bsr->bytes += step;
        left -= step;
        off   = ur_mask_3(bits);

        if ( len_bit <= rest ) {
          out[max] = m & ((1 << len_bit) - 1);
        }
        else {
          l = b[1ULL + max] & ((1 << off) - 1);;
          out[max] = m ^ (l << rest);
        }
      }
    }

    bsr->off   = off;
    bsr->left  = left;
    bsr->bits += len;
  }
}

static inline ur_cue_res_e
_bsr_set_gone(ur_bsr_t *bsr, uint8_t bits)
{
  bsr->bits += bits;
  bsr->bytes = 0;
  bsr->left  = 0;
  bsr->off   = 0;
  return ur_cue_gone;
}

ur_cue_res_e
ur_bsr_tag(ur_bsr_t *bsr, ur_cue_tag_e *out)
{
  uint64_t left = bsr->left;

  if ( !left ) {
    return ur_cue_gone;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint8_t      bit = (b[0] >> off) & 1;
    uint8_t      len = 1;

    if ( 0 == bit ) {
      *out = ur_jam_atom;
    }
    else {
      if ( 7 == off ) {
        if ( 1 == left ) {
          return _bsr_set_gone(bsr, 1);
        }

        bit = b[1] & 1;
      }
      else {
        bit = (b[0] >> (off + 1)) & 1;
      }

      len++;
      *out = ( 0 == bit ) ? ur_jam_cell : ur_jam_back;
    }

    {
      uint8_t  bits = off + len;
      uint8_t bytes = bits >> 3;

      left -= bytes;

      if ( !left ) {
        bsr->bytes = 0;
        bsr->left  = 0;
        bsr->off   = 0;
      }
      else {
        bsr->bytes += bytes;
        bsr->left   = left;
        bsr->off    = ur_mask_3(bits);
      }

      bsr->bits += len;

      return ur_cue_good;
    }
  }
}

static inline ur_cue_res_e
_bsr_rub_log_meme(ur_bsr_t *bsr)
{
  bsr->bits += 256;
  bsr->bytes += 32;
  bsr->left  -= 32;
  return ur_cue_meme;
}

ur_cue_res_e
ur_bsr_rub_log(ur_bsr_t *bsr, uint8_t *out)
{
  uint64_t left = bsr->left;

  if ( !left ) {
    return ur_cue_gone;
  }
  else {
    uint8_t      off = bsr->off;
    uint8_t     rest = 8 - off;
    const uint8_t *b = bsr->bytes;
    uint8_t      byt = b[0] >> off;
    uint8_t     skip = 0;

    while ( !byt ) {
      if ( 32 == skip ) {
        return _bsr_rub_log_meme(bsr);
      }

      skip++;

      if ( skip == left ) {
        return _bsr_set_gone(bsr, (skip << 3) - off);
      }

      byt = b[skip];
    }

    {
      uint32_t zeros = ur_tz8(byt) + (skip ? ((skip << 3) - off) : 0);

      if ( 255 < zeros ) {
        return _bsr_rub_log_meme(bsr);
      }
      else {
        uint32_t bits = off + 1 + zeros;
        uint8_t bytes = bits >> 3;

        left -= bytes;

        bsr->bytes  = left ? (b + bytes) : 0;
        bsr->bits  += 1 + zeros;
        bsr->left   = left;
        bsr->off    = ur_mask_3(bits);

        *out = zeros;
        return ur_cue_good;
      }
    }
  }
}

ur_cue_res_e
ur_bsr_rub_len(ur_bsr_t *bsr, uint64_t *out)
{
  ur_cue_res_e res;
  uint8_t      len;

  if ( ur_cue_good != (res = ur_bsr_rub_log(bsr, &len)) ) {
    return res;
  }
  else if ( 64 <= len ) {
    return ur_cue_meme;
  }

  switch ( len ) {
    case 0: {
      *out = 0;
    } break;

    case 1: {
      *out = 1;
    } break;

    default: {
      len--;
      *out = ur_bsr64_any(bsr, len) ^ (1ULL << len);
    } break;
  }

  return ur_cue_good;
}

void
ur_bsw_grow(ur_bsw_t *bsw, uint64_t step)
{
  uint64_t size = bsw->size;
  uint64_t next = size + step;

  bsw->bytes = realloc(bsw->bytes, next);
  assert(bsw->bytes);
  memset(bsw->bytes + size, 0, step);

  bsw->prev  = size;
  bsw->size  = next;
}

ur_bool_t
ur_bsw_sane(ur_bsw_t *bsw)
{
  return (  (8 > bsw->off)
         && ((bsw->fill << 3) + bsw->off == bsw->bits) );
}

static inline void
_bsw_bit_unsafe(ur_bsw_t *bsw, uint8_t bit)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;

  bsw->bytes[fill] ^= (bit & 1) << off;

  if ( 7 == off ) {
    bsw->fill = 1 + fill;
    bsw->off  = 0;
  }
  else {
    bsw->off  = 1 + off;
  }

  bsw->bits++;
}

void
ur_bsw_bit(ur_bsw_t *bsw, uint8_t bit)
{
  if (  (7 == bsw->off)
     && ((1 + bsw->fill) == bsw->size) )
  {
    ur_bsw_grow(bsw, bsw->prev);
  }

  _bsw_bit_unsafe(bsw, bit);
}

static inline void
_bsw8_unsafe(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;
  uint8_t rest = 8 - off;
  uint8_t l, m;

  //  the least-significant bits of the input become the
  //  most-significant bits of a byte in the output stream
  //
  if ( len < rest ) {
    l = byt & ((1 << len) - 1);

    bsw->bytes[fill] ^= l << off;
    bsw->off = off + len;
  }
  //  and vice-versa
  //
  else {
    l = byt & ((1 << rest) - 1);
    m = byt >> rest;

    bsw->bytes[fill++] ^= l << off;
    off = len - rest;
    bsw->bytes[fill]    = m & ((1 << off) - 1);

    bsw->fill = fill;
    bsw->off  = off;
  }

  bsw->bits += len;
}

void
ur_bsw8(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  len = ur_min(8, len);

  if ( bsw->fill + !!((bsw->off + len) >> 3) >= bsw->size ) {
    ur_bsw_grow(bsw, bsw->prev);
  }

  _bsw8_unsafe(bsw, len, byt);
}

static inline void
_bsw32_unsafe(ur_bsw_t *bsw, uint8_t len, uint32_t val)
{
  uint64_t  fill = bsw->fill;
  uint8_t    off = bsw->off;
  uint8_t *bytes = bsw->bytes;

  bsw->bits += len;

  if ( off ) {
    uint8_t rest = 8 - off;

    if ( len < rest ) {
      bytes[fill] ^= (val & ((1 << len) - 1)) << off;
      bsw->off = off + len;
      return;
    }

    bytes[fill++] ^= (val & ((1 << rest) - 1)) << off;
    val >>= rest;
    len  -= rest;
  }

  switch ( len >> 3 ) {
    case 4: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);

      //  no offset is possible here
      //
      bsw->fill = fill;
      bsw->off  = 0;
      return;
    }

    case 3: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      val >>= 24;
    } break;

    case 2: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      val >>= 16;
    } break;

    case 1: {
      bytes[fill++] = ur_mask_8(val);
      val >>= 8;
    } break;
  }

  off = ur_mask_3(len);

  if ( off ) {
    bytes[fill] = (uint8_t)(val & ((1 << off) - 1));
  }

  bsw->fill = fill;
  bsw->off  = off;
}

void
ur_bsw32(ur_bsw_t *bsw, uint8_t len, uint32_t val)
{
  uint8_t bits, need;

  len  = ur_min(32, len);
  bits = bsw->off + len;
  need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw32_unsafe(bsw, len, val);
}

static inline void
_bsw64_unsafe(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint64_t  fill = bsw->fill;
  uint8_t    off = bsw->off;
  uint8_t *bytes = bsw->bytes;

  bsw->bits += len;

  if ( off ) {
    uint8_t rest = 8 - off;

    if ( len < rest ) {
      bytes[fill] ^= (val & ((1 << len) - 1)) << off;
      bsw->off = off + len;
      return;
    }

    bytes[fill++] ^= (val & ((1 << rest) - 1)) << off;
    val >>= rest;
    len  -= rest;
  }

  switch ( len >> 3 ) {
    case 8: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);
      bytes[fill++] = ur_mask_8(val >> 32);
      bytes[fill++] = ur_mask_8(val >> 40);
      bytes[fill++] = ur_mask_8(val >> 48);
      bytes[fill++] = ur_mask_8(val >> 56);

      //  no offset is possible here
      //
      bsw->fill = fill;
      bsw->off  = 0;
      return;
    }

    case 7: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);
      bytes[fill++] = ur_mask_8(val >> 32);
      bytes[fill++] = ur_mask_8(val >> 40);
      bytes[fill++] = ur_mask_8(val >> 48);
      val >>= 56;
    } break;

    case 6: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);
      bytes[fill++] = ur_mask_8(val >> 32);
      bytes[fill++] = ur_mask_8(val >> 40);
      val >>= 48;
    } break;

    case 5: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);
      bytes[fill++] = ur_mask_8(val >> 32);
      val >>= 40;
    } break;

    case 4: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      bytes[fill++] = ur_mask_8(val >> 24);
      val >>= 32;
    } break;

    case 3: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      bytes[fill++] = ur_mask_8(val >> 16);
      val >>= 24;
    } break;

    case 2: {
      bytes[fill++] = ur_mask_8(val);
      bytes[fill++] = ur_mask_8(val >>  8);
      val >>= 16;
    } break;

    case 1: {
      bytes[fill++] = ur_mask_8(val);
      val >>= 8;
    } break;
  }

  off = ur_mask_3(len);

  if ( off ) {
    bytes[fill] = (uint8_t)(val & ((1 << off) - 1));
  }

  bsw->fill = fill;
  bsw->off  = off;
}

void
ur_bsw64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint8_t bits, need;

  len  = ur_min(64, len);
  bits = bsw->off + len;
  need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw64_unsafe(bsw, len, val);
}

static inline void
_bsw_bytes_unsafe(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint64_t len_byt = len >> 3;
  uint8_t  len_bit = ur_mask_3(len);
  uint64_t    fill = bsw->fill;
  uint8_t      off = bsw->off;

  if ( !off ) {
    memcpy(bsw->bytes + fill, byt, len_byt);
    fill += len_byt;
    off   = len_bit;

    if ( off ) {
      bsw->bytes[fill] = byt[len_byt] & ((1 << off) - 1);
    }
  }
  //  the least-significant bits of the input become the
  //  most-significant bits of a byte in the output stream, and vice-versa
  //
  else {
    uint8_t rest = 8 - off;
    uint8_t mask = (1 << rest) - 1;
    uint8_t l, m = bsw->bytes[fill];
    uint64_t   i;

    for ( i = 0; i < len_byt; i++ ) {
      l = byt[i] & mask;
      bsw->bytes[fill++] = m ^ (l << off);
      m = byt[i] >> rest;
    }

    if ( len_bit < rest ) {
      l = byt[len_byt] & ((1 << len_bit) - 1);
      bsw->bytes[fill] = m ^ (l << off);
      off += len_bit;
    }
    else {
      l = byt[len_byt] & mask;
      bsw->bytes[fill++] = m ^ (l << off);

      m = byt[len_byt] >> rest;

      off = len_bit - rest;
      bsw->bytes[fill] = m & ((1 << off) - 1);
    }
  }

  bsw->off   = off;
  bsw->fill  = fill;
  bsw->bits += len;
}

void
ur_bsw_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint8_t  bits = len + bsw->off;
  uint64_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_bytes_unsafe(bsw, len, byt);
}

static inline void
_bsw_bex_unsafe(ur_bsw_t *bsw, uint8_t n)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;
  uint32_t bits = n + off;

  fill += bits >> 3;
  off   = ur_mask_3(bits);

  bsw->bytes[fill] ^= 1 << off;

  if ( 7 == off ) {
    bsw->off  = 0;
    bsw->fill = 1 + fill;
  }
  else {
    bsw->off  = 1 + off;
    bsw->fill = fill;
  }

  bsw->bits += 1 + n;
}

void
ur_bsw_bex(ur_bsw_t *bsw, uint8_t n)
{
  uint32_t bits = 1 + n + bsw->off;
  uint8_t  need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_bex_unsafe(bsw, n);
}

static inline void
_bsw_mat64_unsafe(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  if ( 0 == len ) {
    _bsw_bit_unsafe(bsw, 1);
  }
  else {
    {
      uint8_t nel = ur_met0_64(len);
      _bsw_bex_unsafe(bsw, nel);
      _bsw64_unsafe(bsw, nel - 1, len);
    }

    _bsw64_unsafe(bsw, len, val);
  }
}

void
ur_bsw_mat64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint8_t next, bits, need;

  len  = ur_min(64, len);
  next = ( 0 == len ) ? 1 : len + (2 * ur_met0_64(len));
  bits = bsw->off + next;
  need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_mat64_unsafe(bsw, len, val);
}

static inline void
_bsw_mat_bytes_unsafe(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  if ( 0 == len ) {
    _bsw_bit_unsafe(bsw, 1);
  }
  else {
    //  write run-length
    //
    {
      uint8_t nel = ur_met0_64(len);
      _bsw_bex_unsafe(bsw, nel);
      _bsw64_unsafe(bsw, nel - 1, len);
    }

    _bsw_bytes_unsafe(bsw, len, byt);
  }
}

void
ur_bsw_mat_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint64_t next = ( 0 == len ) ? 1 : len + (2 * ur_met0_64(len));
  uint64_t bits = bsw->off + next;
  uint64_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_mat_bytes_unsafe(bsw, len, byt);
}

static inline void
_bsw_back64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  _bsw8_unsafe(bsw, 2, 3);
  _bsw_mat64_unsafe(bsw, len, val);
}

void
ur_bsw_back64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint64_t next = ( 0 == len ) ? 1 : len + (2 * ur_met0_64(len));
  uint64_t bits = 2 + bsw->off + next;
  uint64_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_back64(bsw, len, val);
}

static inline void
_bsw_atom64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  _bsw_bit_unsafe(bsw, 0);
  _bsw_mat64_unsafe(bsw, len, val);
}

void
ur_bsw_atom64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint64_t next = ( 0 == len ) ? 1 : len + (2 * ur_met0_64(len));
  uint64_t bits = 1 + bsw->off + next;
  uint64_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_atom64(bsw, len, val);
}

static inline void
_bsw_atom_bytes_unsafe(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  _bsw_bit_unsafe(bsw, 0);
  _bsw_mat_bytes_unsafe(bsw, len, byt);
}

void
ur_bsw_atom_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint64_t next = ( 0 == len ) ? 1 : len + (2 * ur_met0_64(len));
  uint64_t bits = 1 + bsw->off + next;
  uint64_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_atom_bytes_unsafe(bsw, len, byt);
}

void
ur_bsw_cell(ur_bsw_t *bsw)
{
  uint8_t bits = 2 + bsw->off;
  uint8_t need = (bits >> 3) + !!ur_mask_3(bits);

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw8_unsafe(bsw, 2, 1);
}
