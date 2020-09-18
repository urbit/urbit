#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ur/defs.h"
#include "ur/bitstream.h"

ur_cue_res_e
ur_bsr_init(ur_bsr_t *bsr, uint64_t len, const uint8_t *bytes)
{
  //  check for overflow
  //
  if ( (len << 3) < len ) {
    return ur_cue_meme;
  }

  bsr->left  = len;
  bsr->bytes = bytes;

  return ur_cue_good;
}

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
    uint8_t  off = bsr->off;
    uint8_t rest = 8 - off;
    uint32_t   m = bsr->bytes[0] >> off;

    if ( len < rest ) {
      bsr->off = off + len;
      return m & ((1 << len) - 1);
    }
    else {
      const uint8_t *b;
      uint8_t     mask, len_byt;
      uint32_t       l;

      len -= rest;
      left--;
      b = ++bsr->bytes;

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
        default: assert(0);

        case 4: {
          l = (uint32_t)b[0]
            ^ (uint32_t)b[1] << 8
            ^ (uint32_t)b[2] << 16
            ^ (uint32_t)b[3] << 24;
        } break;

        case 3: {
          l = (uint32_t)b[0]
            ^ (uint32_t)b[1] << 8
            ^ (uint32_t)b[2] << 16;

          if ( mask ) {
            l ^= (uint32_t)(b[3] & mask) << 24;
          }
        } break;

        case 2: {
          l = (uint32_t)b[0]
            ^ (uint32_t)b[1] << 8;

          if ( mask ) {
            l ^= (uint32_t)(b[2] & mask) << 16;
          }
        } break;

        case 1: {
          l = (uint32_t)b[0];

          if ( mask ) {
            l ^= (uint32_t)(b[1] & mask) << 8;
          }
        } break;

        case 0: {
          l = ( mask ) ? (uint32_t)(b[0] & mask) : 0;
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
    uint8_t  off = bsr->off;
    uint8_t rest = 8 - off;
    uint64_t   m = bsr->bytes[0] >> off;

    if ( len < rest ) {
      bsr->off = off + len;
      return m & ((1 << len) - 1);
    }
    else {
      const uint8_t *b;
      uint8_t     mask, len_byt;
      uint64_t       l;

      len -= rest;
      left--;
      b = ++bsr->bytes;

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
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16
            ^ (uint64_t)b[3] << 24
            ^ (uint64_t)b[4] << 32
            ^ (uint64_t)b[5] << 40
            ^ (uint64_t)b[6] << 48
            ^ (uint64_t)b[7] << 56;
        } break;

        case 7: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16
            ^ (uint64_t)b[3] << 24
            ^ (uint64_t)b[4] << 32
            ^ (uint64_t)b[5] << 40
            ^ (uint64_t)b[6] << 48;

          if ( mask ) {
            l ^= (uint64_t)(b[7] & mask) << 56;
          }
        } break;

        case 6: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16
            ^ (uint64_t)b[3] << 24
            ^ (uint64_t)b[4] << 32
            ^ (uint64_t)b[5] << 40;

          if ( mask ) {
            l ^= (uint64_t)(b[6] & mask) << 48;
          }
        } break;

        case 5: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16
            ^ (uint64_t)b[3] << 24
            ^ (uint64_t)b[4] << 32;

          if ( mask ) {
            l ^= (uint64_t)(b[5] & mask) << 40;
          }
        } break;

        case 4: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16
            ^ (uint64_t)b[3] << 24;

          if ( mask ) {
            l ^= (uint64_t)(b[4] & mask) << 32;
          }
        } break;

        case 3: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8
            ^ (uint64_t)b[2] << 16;

          if ( mask ) {
            l ^= (uint64_t)(b[3] & mask) << 24;
          }
        } break;

        case 2: {
          l = (uint64_t)b[0]
            ^ (uint64_t)b[1] << 8;

          if ( mask ) {
            l ^= (uint64_t)(b[2] & mask) << 16;
          }
        } break;

        case 1: {
          l = (uint64_t)b[0];

          if ( mask ) {
            l ^= (uint64_t)(b[1] & mask) << 8;
          }
        } break;

        case 0: {
          l = ( mask ) ? (uint64_t)(b[0] & mask) : 0;
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

  bsr->bits += len;

  if ( !left ) {
    return;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint64_t len_byt = len >> 3;
    uint8_t  len_bit = ur_mask_3(len);
    uint64_t    need = len_byt + !!len_bit;

    if ( !off ) {
      if ( need > left ) {
        memcpy(out, b, left);
        left = 0;
        bsr->bytes = 0;
      }
      else {
        memcpy(out, b, len_byt);
        off = len_bit;

        if ( off ) {
          out[len_byt] = b[len_byt] & ((1 << off) - 1);
        }

        left -= len_byt;
        bsr->bytes = ( left ) ? b + len_byt : 0;
      }
    }
    //  the most-significant bits from a byte in the stream
    //  become the least-significant bits of an output byte, and vice-versa
    //
    else {
      uint8_t  rest = 8 - off;
      uint8_t  mask = (1 << off) - 1;
      uint8_t   byt, l, m = *b >> off;
      uint64_t last = left - 1;

      //  loop over all the bytes we need (or all that remain)
      //
      //    [l] holds [off] bits
      //    [m] holds [rest] bits
      //
      {
        uint64_t  max = ur_min(last, len_byt);
        uint64_t i;

        for ( i = 0; i < max; i++ ) {
          byt    = *++b;
          l      = byt & mask;
          out[i] = m ^ (l << rest);
          m      = byt >> off;
        }
      }

      //  we're reading into or beyond the last byte [bsr]
      //
      //    [m] holds all the remaining bits in [bsr],
      //    but we might not need all of it
      //
      if ( need >= left ) {
        uint8_t bits = len - (last << 3);

        if ( bits < rest ) {
          out[last] = m & ((1 << bits) - 1);
          bsr->bytes = b;
          left = 1;
          off += len_bit;
        }
        else {
          out[last] = m;
          bsr->bytes = 0;
          left = 0;
          off  = 0;
        }
      }
      //  we need less than a byte, but it might span multiple bytes
      //
      else {
        uint8_t bits = off + len_bit;
        uint8_t step = !!(bits >> 3);

        bsr->bytes = b + step;
        left -= len_byt + step;
        off   = ur_mask_3(bits);

        if ( len_bit ) {
          if ( len_bit <= rest ) {
            out[len_byt] = m & ((1 << len_bit) - 1);
          }
          else {
            l = *++b & ((1 << off) - 1);
            out[len_byt] = m ^ (l << rest);
          }
        }
      }
    }

    bsr->off  = off;
    bsr->left = left;
  }
}

void
ur_bsr_skip_any(ur_bsr_t *bsr, uint64_t len)
{
  uint64_t left = bsr->left;

  bsr->bits += len;

  if ( !left ) {
    return;
  }
  else {
    const uint8_t *b = bsr->bytes;
    uint8_t      off = bsr->off;
    uint64_t len_byt = len >> 3;
    uint8_t  len_bit = ur_mask_3(len);
    uint64_t    need = len_byt + !!len_bit;
    uint8_t     rest = 8 - off;
    uint64_t    last = left - 1;

    b += ur_min(last, len_byt) + 1;

    if ( need >= left ) {
      uint8_t bits = len - (last << 3);

      if ( bits < rest ) {
        bsr->bytes = b - 1;
        left = 1;
        off += len_bit;
      }
      else {
        bsr->bytes = 0;
        left = 0;
        off  = 0;
      }
    }
    else {
      uint8_t bits = off + len_bit;
      uint8_t step = !!(bits >> 3);

      bsr->bytes = b - (1 - step);
      left -= len_byt + step;
      off   = ur_mask_3(bits);
    }

    bsr->off  = off;
    bsr->left = left;
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
_bsr_log_meme(ur_bsr_t *bsr)
{
  bsr->bits += 256;
  bsr->bytes += 32;
  bsr->left  -= 32;
  return ur_cue_meme;
}

ur_cue_res_e
ur_bsr_log(ur_bsr_t *bsr, uint8_t *out)
{
  uint64_t left = bsr->left;

  if ( !left ) {
    return ur_cue_gone;
  }
  else {
    uint8_t      off = bsr->off;
    const uint8_t *b = bsr->bytes;
    uint8_t      byt = b[0] >> off;
    uint8_t     skip = 0;

    while ( !byt ) {
      if ( 32 == skip ) {
        return _bsr_log_meme(bsr);
      }

      byt = b[++skip];

      if ( skip == left ) {
        return _bsr_set_gone(bsr, (skip << 3) - off);
      }
    }

    {
      uint32_t zeros = ur_tz8(byt) + (skip ? ((skip << 3) - off) : 0);

      if ( 255 < zeros ) {
        return _bsr_log_meme(bsr);
      }
      else {
        uint32_t bits = off + 1 + zeros;
        uint8_t bytes = bits >> 3;

        left -= bytes;

        bsr->bytes = left ? (b + bytes) : 0;
        bsr->bits += 1 + zeros;
        bsr->left  = left;
        bsr->off   = ur_mask_3(bits);

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

  if ( ur_cue_good != (res = ur_bsr_log(bsr, &len)) ) {
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

/*
**  bitstream-writer operations follow a pattern of an unsafe (inline)
**  implementation, unsafe wrt to buffer size and reallocation,
**  wrapped in a public function with buffer size checks.
**
**  higher-level operations made up of multiple discrete writes check
**  the buffer size once for all involved writes.
**
**  this pattern should be easily adaptable to an alternate bitstream-writer
**  implementation that flushes accumulated output periodically instead
**  of reallocating the output buffer.
*/

void
ur_bsw_init(ur_bsw_t *bsw, uint64_t prev, uint64_t size)
{
  bsw->prev  = prev;
  bsw->size  = size;
  bsw->bytes = calloc(size, 1);

  if ( !bsw->bytes ) {
    fprintf(stderr,
            "ur: bitstream-init allocation failed, out of memory\r\n");
    abort();
  }
}

void
ur_bsw_grow(ur_bsw_t *bsw, uint64_t step)
{
  uint64_t size = bsw->size;
  uint64_t next = size + step;

  bsw->bytes = realloc(bsw->bytes, next);

  if ( !bsw->bytes ) {
    fprintf(stderr,
            "ur: bitstream-write allocation failed, out of memory\r\n");
    abort();
  }

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

uint64_t
ur_bsw_done(ur_bsw_t *bsw, uint64_t *len, uint8_t **byt)
{
  uint64_t bits = bsw->bits;

  *len = bsw->fill + !!bsw->off;
  *byt = bsw->bytes;

  memset(bsw, 0, sizeof(*bsw));

  return bits;
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
  uint8_t need;

  len  = ur_min(32, len);
  need = ur_bloq_up3( bsw->off + len );

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
  uint8_t need;

  len  = ur_min(64, len);
  need = ur_bloq_up3( bsw->off + len );

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

    //  no trailing bits; we need only write the rest of the last byte.
    //
    //    NB: while semantically equivalent to the subsequent block,
    //    this case must be separate to avoid reading off the end of [byt]
    //
    if ( !len_bit ) {
      bsw->bytes[fill] = m;
    }
    //  trailing bits fit into the current output byte.
    //
    else if ( len_bit < rest ) {
      l = byt[len_byt] & ((1 << len_bit) - 1);
      bsw->bytes[fill] = m ^ (l << off);
      off += len_bit;
    }
    //  trailing bits extend into the next output byte.
    //
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
  uint64_t need = ur_bloq_up3(len + bsw->off);

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
  uint64_t need = ur_bloq_up3(1 + n + bsw->off);

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

/*
*  the length of a "mat" run-length encoded atom of [len] bits
*/
#define MAT_LEN(len)  ( ( 0 == len ) ? 1 : len + (2 * ur_met0_64((uint64_t)len)) )

void
ur_bsw_mat64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint8_t need;

  len  = ur_min(64, len);
  need = ur_bloq_up3( bsw->off + MAT_LEN(len) );

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
  uint64_t need = ur_bloq_up3( bsw->off + MAT_LEN(len) );

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
  uint8_t need;

  len  = ur_min(64, len);
  need = ur_bloq_up3( 2 + bsw->off + MAT_LEN(len) );

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
  uint8_t need;

  len  = ur_min(64, len);
  need = ur_bloq_up3( 1 + bsw->off + MAT_LEN(len) );

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
  uint64_t need = ur_bloq_up3( 1 + bsw->off + MAT_LEN(len) );

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw_atom_bytes_unsafe(bsw, len, byt);
}

void
ur_bsw_cell(ur_bsw_t *bsw)
{
  uint8_t need = ur_bloq_up3( 2 + bsw->off );

  if ( bsw->fill + need >= bsw->size ) {
    ur_bsw_grow(bsw, ur_max(need, bsw->prev));
  }

  _bsw8_unsafe(bsw, 2, 1);
}
