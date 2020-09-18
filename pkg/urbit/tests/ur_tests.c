#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "ur/ur.h"

/*
**  initialize helper for bitstream-writer tests.
*/
static void
_bsw_reinit(ur_bsw_t *bsw, uint64_t prev, uint64_t size)
{
  bsw->prev = prev;
  bsw->size = size;
  bsw->bits = 0;
  bsw->fill = 0;
  bsw->off  = 0;

  free(bsw->bytes);
  bsw->bytes = calloc(size, 1);
}

/*
**  check bitstream-writer test invariants.
*/
static int
_bsw_bit_check(const char* cap, ur_bsw_t *bsw, uint8_t byt, uint8_t off)
{
  int ret = 1;

  if ( !ur_bsw_sane(bsw) ) {
    fprintf(stderr, "%s: insane off=%u fill=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, bsw->off, bsw->fill, bsw->bits);
    ret = 0;
  }

  if ( byt != bsw->bytes[0] ) {
    fprintf(stderr, "%s: bytes fail (%u, %u)\r\n", cap, byt, bsw->bytes[0]);
    ret = 0;
  }

  if ( off != bsw->off ) {
    fprintf(stderr, "%s: offset fail (%u, %u)\r\n", cap, off, bsw->off);
    ret = 0;
  }

  return ret;
}

/*
**  test 8 sequential writes of a set bit.
*/
static int
_test_bsw_bit_ones(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_reinit(&bsw, 1, 1);

  ret &= _bsw_bit_check("bsw ones init", &bsw, 0x0, 0);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones a", &bsw, 0x1, 1);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones b", &bsw, 0x3, 2);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones c", &bsw, 0x7, 3);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones d", &bsw, 0xf, 4);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones e", &bsw, 0x1f, 5);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones f", &bsw, 0x3f, 6);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones g", &bsw, 0x7f, 7);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw ones h", &bsw, 0xff, 0);

  if ( bsw.size != 2 ) {
    fprintf(stderr, "bsw ones grow: fail\r\n");
    ret = 0;
  }

  free(bsw.bytes);

  return ret;
}

/*
**  test 8 sequential writes of 1 null bit.
*/
static int
_test_bsw_bit_zeros(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_reinit(&bsw, 1, 1);

  ret &= _bsw_bit_check("bsw zeros init", &bsw, 0x0, 0);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros a", &bsw, 0x0, 1);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros b", &bsw, 0x0, 2);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros c", &bsw, 0x0, 3);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros d", &bsw, 0x0, 4);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros e", &bsw, 0x0, 5);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros f", &bsw, 0x0, 6);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros g", &bsw, 0x0, 7);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw zeros h", &bsw, 0x0, 0);

  if ( bsw.size != 2 ) {
    fprintf(stderr, "bsw zeros grow: fail\r\n");
    ret = 0;
  }

  free(bsw.bytes);

  return ret;
}

/*
**  test 8 sequential writes of alternating bits.
*/
static int
_test_bsw_bit_alt(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_reinit(&bsw, 1, 1);

  ret &= _bsw_bit_check("bsw alt init", &bsw, 0x0, 0);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw alt a", &bsw, 0x0, 1);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw alt b", &bsw, 0x2, 2);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw alt c", &bsw, 0x2, 3);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw alt d", &bsw, 0xa, 4);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw alt e", &bsw, 0xa, 5);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw alt f", &bsw, 0x2a, 6);

  ur_bsw_bit(&bsw, 0);
  ret &= _bsw_bit_check("bsw alt g", &bsw, 0x2a, 7);

  ur_bsw_bit(&bsw, 1);
  ret &= _bsw_bit_check("bsw alt h", &bsw, 0xaa, 0);

  if ( bsw.size != 2 ) {
    fprintf(stderr, "bsw alt grow: fail\r\n");
    ret = 0;
  }

  free(bsw.bytes);

  return ret;
}

static int
_test_bsw_bit(void)
{
  return _test_bsw_bit_ones()
       & _test_bsw_bit_zeros()
       & _test_bsw_bit_alt();
}

/*
**  subsequents bitstream-writer tests assume the correctnesss of
**  ur_bsw_bit(), and compare the output of a bit-at-a-time
**  "golden master" with that of the relevant, higher-level operation.
**
**    XX the "golden" master implementations shouldn't be in bitstream module,
**    as we don't intend to run them, but it's kind of weird implement them
**    in the test itself.
**
*/
static int
_bsw_cmp_check(const char* cap, uint8_t val, uint8_t off, uint8_t len, ur_bsw_t *a, ur_bsw_t *b)
{
  int ret = 1;

  if ( !ur_bsw_sane(a) ) {
    fprintf(stderr, "%s: val 0x%02x off %u, len %u: a insane off=%u fill=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, val, off, len, a->off, a->fill, a->bits);
    ret = 0;
  }
  if ( !ur_bsw_sane(b) ) {
    fprintf(stderr, "%s: val 0x%02x off %u len %u: b insane off=%u fill=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, val, off, len, b->off, b->fill, b->bits);
    ret = 0;
  }

  if ( a->off != b->off ) {
    fprintf(stderr, "%s: val 0x%02x off %u len %u: offset fail (%u, %u)\r\n",
                    cap, val, off, len, a->off, b->off);
    ret = 0;
  }

  if ( a->fill != b->fill ) {
    fprintf(stderr, "%s: val 0x%02x off %u len %u: fill fail (%" PRIu64 ", %" PRIu64 ")\r\n",
                    cap, val, off, len, a->fill, b->fill);
    ret = 0;
  }

  {
    uint64_t k, len_byt = a->fill + !!a->off;

    if ( memcmp(a->bytes, b->bytes, len_byt) ) {
      fprintf(stderr, "%s: val 0x%02x off %u, len %u not equal off=%u fill=%" PRIu64 "\r\n",
                      cap, val, off, len, b->off, b->fill);
      fprintf(stderr, "  a: { ");
      for ( k = 0; k < len_byt; k++ ) {
        fprintf(stderr, "%02x, ", a->bytes[k]);
      }
      fprintf(stderr, "}\r\n");
      fprintf(stderr, "  b: { ");
      for ( k = 0; k < len_byt; k++ ) {
        fprintf(stderr, "%02x, ", b->bytes[k]);
      }
      fprintf(stderr, "}\r\n");
      ret = 0;
    }
  }

  return ret;
}

/*
**  ur_bsw8 golden master
*/
static void
_bsw8_slow(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  len = ur_min(8, len);

  while ( len ) {
    ur_bsw_bit(bsw, byt);
    byt >>= 1;
    len--;
  }
}

/*
**  at varying offsets, write varying numbers of bits via
**  ur_bsw8 and master, comparing the result each time.
*/
static int
_test_bsw8_loop(const char* cap, uint8_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 8; j++ ) {
      _bsw_reinit(&a, 1, 1);
      _bsw_reinit(&b, 1, 1);
      a.off = a.bits = b.off = b.bits = i;

      _bsw8_slow(&a, j, val);
      ur_bsw8(&b, j, val);

      ret &= _bsw_cmp_check(cap, val, i, j, &a, &b);
    }
  }

  return ret;
}

static int
_test_bsw8(void)
{
  return _test_bsw8_loop("bsw bits ones", 0xff)
       & _test_bsw8_loop("bsw bits zeros", 0x0)
       & _test_bsw8_loop("bsw bits alt 1", 0xaa)
       & _test_bsw8_loop("bsw bits alt 2", 0x55);
}

/*
**  ur_bsw32 golden master
*/
static void
_bsw32_slow(ur_bsw_t *bsw, uint8_t len, uint32_t val)
{
  len = ur_min(32, len);

  while ( len ) {
    ur_bsw_bit(bsw, val & 0xff);
    val >>= 1;
    len--;
  }
}

/*
**  at varying offsets, write varying numbers of bits via
**  ur_bsw32 and master, comparing the result each time.
*/
static int
_test_bsw32_loop(const char* cap, uint32_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 32; j++ ) {
      _bsw_reinit(&a, 1, 1);
      _bsw_reinit(&b, 1, 1);
      a.off = a.bits = b.off = b.bits = i;

      _bsw32_slow(&a, j, val);
      ur_bsw32(&b, j, val);

      ret &= _bsw_cmp_check(cap, val, i, j, &a, &b);
    }
  }

  return ret;
}

static int
_test_bsw32(void)
{
  return _test_bsw32_loop("bsw 32 ones", 0xffffffff)
       & _test_bsw32_loop("bsw 32 zeros", 0x0)
       & _test_bsw32_loop("bsw 32 alt 1", 0xaaaaaaaa)
       & _test_bsw32_loop("bsw 32 alt 2", 0x55555555);
}

/*
**  ur_bsw64 golden master
*/
static void
_bsw64_slow(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  len = ur_min(64, len);

  while ( len ) {
    ur_bsw_bit(bsw, val & 0xff);
    val >>= 1;
    len--;
  }
}

/*
**  at varying offsets, write varying numbers of bits via
**  ur_bsw64 and master, comparing the result each time.
*/
static int
_test_bsw64_loop(const char* cap, uint64_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 64; j++ ) {
      _bsw_reinit(&a, 1, 1);
      _bsw_reinit(&b, 1, 1);
      a.off = a.bits = b.off = b.bits = i;

      _bsw64_slow(&a, j, val);
      ur_bsw64(&b, j, val);

      ret &= _bsw_cmp_check(cap, val, i, j, &a, &b);
    }
  }

  return ret;
}

static int
_test_bsw64(void)
{
  return _test_bsw64_loop("bsw 64 ones", 0xffffffffffffffffULL)
       & _test_bsw64_loop("bsw 64 zeros", 0x0ULL)
       & _test_bsw64_loop("bsw 64 alt 1", 0xaaaaaaaaaaaaaaaaULL)
       & _test_bsw64_loop("bsw 64 alt 2", 0x5555555555555555ULL);
}

/*
**  ur_bsw_bytes() golden master
*/
static void
_bsw_bytes_slow(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint64_t i, len_byt = len >> 3;

  for ( i = 0; i < len_byt; i++ ) {
    _bsw8_slow(bsw, 8, byt[i]);
    len -= 8;
  }

  _bsw8_slow(bsw, len, byt[len_byt]);
}

/*
**  at varying offsets, write varying numbers of bits via
**  ur_bsw_bytes and master, comparing the result each time.
*/
static int
_test_bsw_bytes_loop(const char* cap, uint64_t len, uint8_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j, *byt;
  uint64_t   len_bit = len << 3;

  byt = malloc(len);
  memset(byt, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j < len_bit; j++ ) {
      _bsw_reinit(&a, 1, 1);
      _bsw_reinit(&b, 1, 1);
      a.off = a.bits = b.off = b.bits = i;

      _bsw_bytes_slow(&a, j, byt);
      ur_bsw_bytes(&b, j, byt);

      ret &= _bsw_cmp_check(cap, val, i, j, &a, &b);
    }
  }

  free(byt);

  return ret;
}

static int
_test_bsw_bytes(void)
{
  return _test_bsw_bytes_loop("bsw bytes nought", 0, 0x0)
       & _test_bsw_bytes_loop("bsw bytes ones odd", 3, 0xff)
       & _test_bsw_bytes_loop("bsw bytes ones even", 4, 0xff)
       & _test_bsw_bytes_loop("bsw bytes zeros odd", 5, 0x0)
       & _test_bsw_bytes_loop("bsw bytes zeros even", 6, 0x0)
       & _test_bsw_bytes_loop("bsw bytes alt 1 odd", 7, 0xaa)
       & _test_bsw_bytes_loop("bsw bytes alt 1 even", 8, 0xaa)
       & _test_bsw_bytes_loop("bsw bytes alt 2 odd", 9, 0x55)
       & _test_bsw_bytes_loop("bsw bytes alt 2 even", 10, 0x55);
}

/*
**  ur_bsw_bex golden master
*/
static void
_bsw_bex_slow(ur_bsw_t *bsw, uint8_t n)
{
  while ( n >= 64 ) {
    _bsw64_slow(bsw, 64, 0);
    n -= 64;
  }

  _bsw64_slow(bsw, n + 1, 1ULL << n);
}

/*
**  at varying offsets, write varying numbers of bits via
**  ur_bsw_bex and master, comparing the result each time.
*/
static int
_test_bsw_bex()
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t  i;
  uint32_t j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j < 256; j++ ) {
      _bsw_reinit(&a, 1, 1);
      _bsw_reinit(&b, 1, 1);
      a.off = a.bits = b.off = b.bits = i;

      _bsw_bex_slow(&a, j);
      ur_bsw_bex(&b, j);

      ret &= _bsw_cmp_check("bsw bex", j, i, j + 1, &a, &b);
    }
  }

  return ret;
}

static int
_test_bsw(void)
{
  return _test_bsw_bit()
       & _test_bsw8()
       & _test_bsw32()
       & _test_bsw64()
       & _test_bsw_bytes()
       & _test_bsw_bex();
}

/*
**  check bitstream-reader test invariants.
*/
static int
_bsr_bit_check(const char  *cap,
               ur_bsr_t    *bsr,
               uint8_t      off,
               uint64_t    bits,
               uint8_t      exp,
               uint8_t      val,
               ur_cue_res_e ser,
               ur_cue_res_e res)
{
  int ret = 1;

  if ( !ur_bsr_sane(bsr) ) {
    fprintf(stderr, "%s: insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, bsr->off, bsr->left, bsr->bits);
    ret = 0;
  }

  if ( ser != res ) {
    fprintf(stderr, "%s: res not equal (%s, %s) off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                    cap, (ur_cue_good == ser) ? "good" : "gone",
                    (ur_cue_good == res) ? "good" : "gone",
                    bsr->off, bsr->left, bsr->left ? bsr->bytes[0] : 0, bsr->bits);
    ret = 0;
  }

  if ( (ur_cue_good == res) && (exp != val) ) {
    fprintf(stderr, "%s: val not equal (%02x, %02x) off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                    cap, exp, val, bsr->off, bsr->left, bsr->left ? bsr->bytes[0] : 0, bsr->bits);
    ret = 0;
  }

  if ( off != bsr->off ) {
    fprintf(stderr, "%s: offset fail (%u, %u)\r\n", cap, off, bsr->off);
    ret = 0;
  }

  if ( bits != bsr->bits ) {
    fprintf(stderr, "%s: bits fail (%" PRIu64 ", %" PRIu64 ")\r\n", cap, bits, bsr->bits);
    ret = 0;
  }

  return ret;
}

/*
**  read a bit 8 times from a bitstream initialized to all ones,
**  checking invariants and result after each read.
*/
static int
_test_bsr_bit_ones(void)
{
  int          ret    = 1;
  uint8_t     ones[1] = { 0xff };
  ur_bsr_t     bsr    = { .left = sizeof(ones), .bytes = ones };
  uint8_t      out;
  ur_cue_res_e res;

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 1", &bsr, 1, 1, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 2", &bsr, 2, 2, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 3", &bsr, 3, 3, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 4", &bsr, 4, 4, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 5", &bsr, 5, 5, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 6", &bsr, 6, 6, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 7", &bsr, 7, 7, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 8", &bsr, 0, 8, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 9", &bsr, 0, 8, ur_cue_gone, res, 0, 0);

  return ret;
}

/*
**  read a bit 8 times from a bitstream initialized to all zeros,
**  checking invariants and result after each read.
*/
static int
_test_bsr_bit_zeros(void)
{
  int          ret    = 1;
  uint8_t     ones[1] = { 0x0 };
  ur_bsr_t     bsr    = { .left = sizeof(ones), .bytes = ones };
  uint8_t      out;
  ur_cue_res_e res;

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 1", &bsr, 1, 1, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 2", &bsr, 2, 2, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 3", &bsr, 3, 3, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 4", &bsr, 4, 4, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 5", &bsr, 5, 5, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 6", &bsr, 6, 6, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 7", &bsr, 7, 7, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 8", &bsr, 0, 8, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 9", &bsr, 0, 8, ur_cue_gone, res, 0, 0);

  return ret;
}

/*
**  read a bit 8 times from a bitstream initialized to alternating zeros and ones,
**  checking invariants and result after each read.
*/
static int
_test_bsr_bit_alt(void)
{
  int          ret    = 1;
  uint8_t     ones[1] = { 0xaa };
  ur_bsr_t     bsr    = { .left = sizeof(ones), .bytes = ones };
  uint8_t      out;
  ur_cue_res_e res;

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 1", &bsr, 1, 1, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 2", &bsr, 2, 2, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 3", &bsr, 3, 3, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 4", &bsr, 4, 4, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 5", &bsr, 5, 5, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 6", &bsr, 6, 6, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 7", &bsr, 7, 7, ur_cue_good, res, 0, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 8", &bsr, 0, 8, ur_cue_good, res, 1, out);

  res  = ur_bsr_bit(&bsr, &out);
  ret &= _bsr_bit_check("bsr bit ones 9", &bsr, 0, 8, ur_cue_gone, res, 0, 0);

  return ret;
}

static int
_test_bsr_bit(void)
{
  return _test_bsr_bit_ones()
       & _test_bsr_bit_zeros()
       & _test_bsr_bit_alt();
}

/*
**  check bitstream-reader test invariants, after (maybe) reading
**  of the end of the stream.
*/
static int
_bsr_bit_any_check(const char* cap, ur_bsr_t *bsr, uint8_t off, uint64_t bits, uint8_t exp, uint8_t val)
{
  int ret = 1;

  if ( !ur_bsr_sane(bsr) ) {
    fprintf(stderr, "%s: insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, bsr->off, bsr->left, bsr->bits);
    ret = 0;
  }

  if ( exp != val ) {
    fprintf(stderr, "%s: not equal (%02x, %02x) off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                    cap, exp, val, bsr->off, bsr->left, bsr->left ? bsr->bytes[0] : 0, bsr->bits);
    ret = 0;
  }

  if ( off != bsr->off ) {
    fprintf(stderr, "%s: offset fail (%u, %u)\r\n", cap, off, bsr->off);
    ret = 0;
  }

  if ( bits != bsr->bits ) {
    fprintf(stderr, "%s: bits fail (%" PRIu64 ", %" PRIu64 ")\r\n", cap, bits, bsr->bits);
    ret = 0;
  }

  return ret;
}

/*
**  read a bit 17 times from a bitstream initialized to 8 ones,
**  checking invariants and result after each read.
*/
static int
_test_bsr_bit_any_ones(void)
{
  int      ret     = 1;
  uint8_t  ones[1] = { 0xff };
  ur_bsr_t bsr     = { .left = sizeof(ones), .bytes = ones };
  uint8_t  out;

  ret &= _bsr_bit_any_check("bsr bit-any ones init", &bsr, 0, 0, 0, 0);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 1", &bsr, 1, 1, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 2", &bsr, 2, 2, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 3", &bsr, 3, 3, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 4", &bsr, 4, 4, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 5", &bsr, 5, 5, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 6", &bsr, 6, 6, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 7", &bsr, 7, 7, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones 8", &bsr, 0, 8, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 9", &bsr, 0, 9, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 10", &bsr, 0, 10, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 11", &bsr, 0, 11, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 12", &bsr, 0, 12, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 13", &bsr, 0, 13, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 14", &bsr, 0, 14, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 15", &bsr, 0, 15, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 16", &bsr, 0, 16, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any ones off 17", &bsr, 0, 17, 0, out);

  return ret;
}

/*
**  read a bit 17 times from a bitstream initialized to 8 zeros,
**  checking invariants and result after each read.
*/
static int
_test_bsr_bit_any_zeros(void)
{
  int      ret     = 1;
  uint8_t  ones[1] = { 0x0 };
  ur_bsr_t bsr     = { .left = sizeof(ones), .bytes = ones };
  uint8_t  out;

  ret &= _bsr_bit_any_check("bsr bit-any zeros init", &bsr, 0, 0, 0, 0);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 1", &bsr, 1, 1, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 2", &bsr, 2, 2, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 3", &bsr, 3, 3, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 4", &bsr, 4, 4, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 5", &bsr, 5, 5, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 6", &bsr, 6, 6, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 7", &bsr, 7, 7, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros 8", &bsr, 0, 8, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 9", &bsr, 0, 9, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 10", &bsr, 0, 10, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 11", &bsr, 0, 11, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 12", &bsr, 0, 12, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 13", &bsr, 0, 13, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 14", &bsr, 0, 14, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 15", &bsr, 0, 15, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 16", &bsr, 0, 16, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any zeros off 17", &bsr, 0, 17, 0, out);

  return ret;
}

/*
**  read a bit 17 times from a bitstream initialized to 8 bits of alternating,
**  ones and zeros, checking invariants and result after each read.
*/
static int
_test_bsr_bit_any_alt(void)
{
  int      ret     = 1;
  uint8_t  ones[1] = { 0xaa };
  ur_bsr_t bsr     = { .left = sizeof(ones), .bytes = ones };
  uint8_t  out;

  ret &= _bsr_bit_any_check("bsr bit-any alt init", &bsr, 0, 0, 0, 0);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 1", &bsr, 1, 1, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 2", &bsr, 2, 2, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 3", &bsr, 3, 3, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 4", &bsr, 4, 4, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 5", &bsr, 5, 5, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 6", &bsr, 6, 6, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 7", &bsr, 7, 7, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt 8", &bsr, 0, 8, 1, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 9", &bsr, 0, 9, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 10", &bsr, 0, 10, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 11", &bsr, 0, 11, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 12", &bsr, 0, 12, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 13", &bsr, 0, 13, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 14", &bsr, 0, 14, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 15", &bsr, 0, 15, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 16", &bsr, 0, 16, 0, out);

  out = ur_bsr_bit_any(&bsr);
  ret &= _bsr_bit_any_check("bsr bit-any alt off 17", &bsr, 0, 17, 0, out);

  return ret;
}

static int
_test_bsr_bit_any(void)
{
  return _test_bsr_bit_any_ones()
       & _test_bsr_bit_any_zeros()
       & _test_bsr_bit_any_alt();
}

/*
**  subsequents bitstream-reader tests assume the correctnesss of
**  ur_bsr_bit_any(), and compare the output of a bit-at-a-time
**  "golden master" with that of the relevant, higher-level operation.
**
**    XX the "golden" master implementations shouldn't be in bitstream module,
**    as we don't intend to run them, but it's kind of weird implement them
**    in the test itself.
**
*/
static int
_bsr_cmp_any_check(const char* cap, uint8_t off, uint8_t len, ur_bsr_t *a, ur_bsr_t *b)
{
  int ret = 1;

  if ( !ur_bsr_sane(a) ) {
    fprintf(stderr, "%s: off %u, len %u a insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len, a->off, a->left, a->bits);
    ret = 0;
  }

  if ( !ur_bsr_sane(b) ) {
    fprintf(stderr, "%s: off %u, len %u a insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len, b->off, b->left, b->bits);
    ret = 0;
  }

  if ( a->off != b->off ) {
    fprintf(stderr, "%s: off %u len %u: offset fail (%u, %u)\r\n",
                    cap, off, len, a->off, b->off);
    ret = 0;
  }

  if ( a->left != b->left ) {
    fprintf(stderr, "%s: off %u len %u: left fail (%" PRIu64 ", %" PRIu64 ")\r\n",
                    cap, off, len, a->left, b->left);
    ret = 0;
  }

  if ( a->bits != b->bits ) {
    fprintf(stderr, "%s: off %u len %u: bits fail (%" PRIu64 ", %" PRIu64 ")\r\n",
                    cap, off, len, a->bits, b->bits);
    ret = 0;
  }

  if ( a->bytes != b->bytes ) {
    fprintf(stderr, "%s: off %u len %u: bytes fail (%p, %p)\r\n",
                    cap, off, len, a->bytes, b->bytes);
    ret = 0;
  }

  return ret;
}

/*
**  ur_bsr8_any golden master
*/
static uint8_t
_bsr8_any_slow(ur_bsr_t *bsr, uint8_t len)
{
  uint8_t i, out = 0;

  len = ur_min(8, len);

  for ( i = 0; i < len; i++ ) {
    out ^= ur_bsr_bit_any(bsr) << i;
  }

  return out;
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a varying numbers of bits via ur_bsr8_any and master, comparing
**  the results and respective states each time.
*/
static int
_test_bsr8_loop(const char *cap, uint8_t len, uint8_t val)
{
  int         ret = 1;
  uint8_t *bytes;
  ur_bsr_t a, b;
  uint8_t  c, d, i, j;

  bytes = malloc(len);
  memset(bytes, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 8; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = bytes;
      a.off = a.bits = b.off = b.bits = i;

      c = _bsr8_any_slow(&a, j);
      d = ur_bsr8_any(&b, j);

      ret &= _bsr_cmp_any_check(cap, i, j, &a, &b);

      if ( c != d ) {
        fprintf(stderr, "%s: off %u, len %u not equal (%02x, %02x) off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                        cap, i, j, c, d, b.off, b.left, b.left ? b.bytes[0] : 0, b.bits);
        ret = 0;
      }
    }
  }

  free(bytes);

  return ret;
}

static int
_test_bsr8(void)
{
  return _test_bsr8_loop("bsr8 ones 1", 1, 0xff)
       & _test_bsr8_loop("bsr8 ones 2", 2, 0xff)
       & _test_bsr8_loop("bsr8 zeros 1", 1, 0x0)
       & _test_bsr8_loop("bsr8 zeros 2", 2, 0x0)
       & _test_bsr8_loop("bsr8 alt-1 1", 1, 0xaa)
       & _test_bsr8_loop("bsr8 alt-1 2", 2, 0xaa)
       & _test_bsr8_loop("bsr8 alt-2 1", 1, 0x55)
       & _test_bsr8_loop("bsr8 alt-2 2", 2, 0x55);
}

/*
**  ur_bsr32_any golden master
*/
static uint32_t
_bsr32_any_slow(ur_bsr_t *bsr, uint8_t len)
{
  uint32_t out = 0;
  uint8_t    i;

  len = ur_min(32, len);

  for ( i = 0; i < len; i++ ) {
    out ^= (uint32_t)ur_bsr_bit_any(bsr) << i;
  }

  return out;
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a varying numbers of bits via ur_bsr32_any and master, comparing
**  the results and respective states each time.
*/
static int
_test_bsr32_loop(const char *cap, uint8_t len, uint8_t val)
{
  int         ret = 1;
  uint8_t *bytes;
  ur_bsr_t a, b;
  uint32_t c, d;
  uint8_t  i, j;

  bytes = malloc(len);
  memset(bytes, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 32; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = bytes;
      a.off = a.bits = b.off = b.bits = i;

      c = _bsr32_any_slow(&a, j);
      d = ur_bsr32_any(&b, j);

      ret &= _bsr_cmp_any_check(cap, i, j, &a, &b);

      if ( c != d ) {
        fprintf(stderr, "%s: off %u, len %u not equal (%08x, %08x) off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                        cap, i, j, c, d, b.off, b.left, b.left ? b.bytes[0] : 0, b.bits);
        ret = 0;
      }
    }
  }

  free(bytes);

  return ret;
}

static int
_test_bsr32(void)
{
  return _test_bsr32_loop("bsr32 ones 1", 1, 0xff)
       & _test_bsr32_loop("bsr32 ones 2", 2, 0xff)
       & _test_bsr32_loop("bsr32 ones 3", 3, 0xff)
       & _test_bsr32_loop("bsr32 ones 4", 4, 0xff)
       & _test_bsr32_loop("bsr32 zeros 1", 1, 0x0)
       & _test_bsr32_loop("bsr32 zeros 2", 2, 0x0)
       & _test_bsr32_loop("bsr32 zeros 3", 3, 0x0)
       & _test_bsr32_loop("bsr32 zeros 4", 4, 0x0)
       & _test_bsr32_loop("bsr32 alt-1 1", 1, 0xaa)
       & _test_bsr32_loop("bsr32 alt-1 2", 2, 0xaa)
       & _test_bsr32_loop("bsr32 alt-1 3", 3, 0xaa)
       & _test_bsr32_loop("bsr32 alt-1 4", 4, 0xaa)
       & _test_bsr32_loop("bsr32 alt-2 1", 1, 0x55)
       & _test_bsr32_loop("bsr32 alt-2 2", 2, 0x55)
       & _test_bsr32_loop("bsr32 alt-2 3", 3, 0x55)
       & _test_bsr32_loop("bsr32 alt-2 4", 4, 0x55);
}

/*
**  ur_bsr64_any golden master
*/
static uint64_t
_bsr64_any_slow(ur_bsr_t *bsr, uint8_t len)
{
  uint64_t out = 0;
  uint8_t    i;

  len = ur_min(64, len);

  for ( i = 0; i < len; i++ ) {
    out ^= (uint64_t)ur_bsr_bit_any(bsr) << i;
  }

  return out;
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a varying numbers of bits via ur_bsr64_any and master, comparing
**  the results and respective states each time.
*/
static int
_test_bsr64_loop(const char *cap, uint8_t len, uint8_t val)
{
  int         ret = 1;
  uint8_t *bytes;
  ur_bsr_t a, b;
  uint64_t c, d;
  uint8_t  i, j;

  bytes = malloc(len);
  memset(bytes, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 64; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = bytes;
      a.off = a.bits = b.off = b.bits = i;

      c = _bsr64_any_slow(&a, j);
      d = ur_bsr64_any(&b, j);

      ret &= _bsr_cmp_any_check(cap, i, j, &a, &b);

      if ( c != d ) {
        fprintf(stderr, "%s: off %u, len %u not equal (%016" PRIx64", %016" PRIx64") off=%u left=%" PRIu64 " byte=%02x bits=%" PRIu64 "\r\n",
                        cap, i, j, c, d, b.off, b.left, b.left ? b.bytes[0] : 0, b.bits);
        ret = 0;
      }
    }
  }

  free(bytes);

  return ret;
}

static int
_test_bsr64(void)
{
  return _test_bsr64_loop("bsr64 ones 1", 1, 0xff)
       & _test_bsr64_loop("bsr64 ones 2", 2, 0xff)
       & _test_bsr64_loop("bsr64 ones 3", 3, 0xff)
       & _test_bsr64_loop("bsr64 ones 4", 4, 0xff)
       & _test_bsr64_loop("bsr64 ones 5", 5, 0xff)
       & _test_bsr64_loop("bsr64 ones 6", 6, 0xff)
       & _test_bsr64_loop("bsr64 ones 7", 7, 0xff)
       & _test_bsr64_loop("bsr64 ones 8", 8, 0xff)
       & _test_bsr64_loop("bsr64 zeros 1", 1, 0x0)
       & _test_bsr64_loop("bsr64 zeros 2", 2, 0x0)
       & _test_bsr64_loop("bsr64 zeros 3", 3, 0x0)
       & _test_bsr64_loop("bsr64 zeros 4", 4, 0x0)
       & _test_bsr64_loop("bsr64 zeros 5", 5, 0x0)
       & _test_bsr64_loop("bsr64 zeros 6", 6, 0x0)
       & _test_bsr64_loop("bsr64 zeros 7", 7, 0x0)
       & _test_bsr64_loop("bsr64 zeros 8", 8, 0x0)
       & _test_bsr64_loop("bsr64 alt-1 1", 1, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 2", 2, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 3", 3, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 4", 4, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 5", 5, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 6", 6, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 7", 7, 0xaa)
       & _test_bsr64_loop("bsr64 alt-1 8", 8, 0xaa)
       & _test_bsr64_loop("bsr64 alt-2 1", 1, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 2", 2, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 3", 3, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 4", 4, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 5", 5, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 6", 6, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 7", 7, 0x55)
       & _test_bsr64_loop("bsr64 alt-2 8", 8, 0x55);
}

/*
**  ur_bsr_bytes_any golden master
*/
static void
_bsr_bytes_any_slow(ur_bsr_t *bsr, uint64_t len, uint8_t *out)
{
  uint64_t i, len_byt = len >> 3, len_bit = ur_mask_3(len);

  for ( i = 0; i < len_byt; i++ ) {
    out[i] = _bsr8_any_slow(bsr, 8);
  }

  if ( len_bit ) {
    out[len_byt] = _bsr8_any_slow(bsr, len_bit);
  }
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a varying numbers of bits via ur_bsr_bytes_any and master, comparing
**  the results and respective states each time.
*/
static int
_test_bsr_bytes_any_loop(const char *cap, uint8_t len, uint8_t val)
{
  int        ret = 1;
  uint64_t   max = (len << 3) + 7;
  ur_bsr_t  a, b;
  uint8_t *bytes, *c, *d;
  uint8_t   i, j, k;

  c     = malloc(1 + len);
  d     = malloc(1 + len);
  bytes = malloc(len);
  memset(bytes, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 1; j <= max; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = len ? bytes : 0;
      a.off   = b.off   = len ? i     : 0;
      a.bits  = b.bits  = i;
      memset(c, 0x0, len);
      memset(d, 0x0, len);

      _bsr_bytes_any_slow(&a, j, c);
      ur_bsr_bytes_any(&b, j, d);

      ret &= _bsr_cmp_any_check(cap, i, j, &a, &b);

      if ( memcmp(c, d, len) ) {
        fprintf(stderr, "%s: off %u, len %u not equal off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                        cap, i, j, b.off, b.left, b.bits);
        fprintf(stderr, "  a: { ");
        for ( k = 0; k < len; k++ ) {
          fprintf(stderr, "%02x, ", c[k]);
        }
        fprintf(stderr, "}\r\n");
        fprintf(stderr, "  b: { ");
        for ( k = 0; k < len; k++ ) {
          fprintf(stderr, "%02x, ", d[k]);
        }
        fprintf(stderr, "}\r\n");
        ret = 0;
      }
    }
  }

  free(bytes);
  free(d);
  free(c);

  return ret;
}

static int
_test_bsr_bytes_any(void)
{
  return _test_bsr_bytes_any_loop("bsr bytes nought", 0, 0x0)
       & _test_bsr_bytes_any_loop("bsr bytes ones odd", 3, 0xff)
       & _test_bsr_bytes_any_loop("bsr bytes ones even", 4, 0xff)
       & _test_bsr_bytes_any_loop("bsr bytes zeros odd", 5, 0x0)
       & _test_bsr_bytes_any_loop("bsr bytes zeros even", 6, 0x0)
       & _test_bsr_bytes_any_loop("bsr bytes alt 1 odd", 7, 0xaa)
       & _test_bsr_bytes_any_loop("bsr bytes alt 1 even", 8, 0xaa)
       & _test_bsr_bytes_any_loop("bsr bytes alt 2 odd", 9, 0x55)
       & _test_bsr_bytes_any_loop("bsr bytes alt 2 even", 10, 0x55);
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  skip a varying numbers of bits via ur_bsr_skip_any and read the same via
**  ur_bsr_bytes_any master, comparing the respective states each time.
*/
static int
_test_bsr_skip_any_loop(const char *cap, uint8_t len, uint8_t val)
{
  int        ret = 1;
  uint64_t   max = (len << 3) + 7;
  ur_bsr_t  a, b;
  uint8_t *bytes, *c;
  uint8_t   i, j;

  c     = malloc(1 + len);
  bytes = malloc(len);
  memset(bytes, val, len);

  for ( i = 0; i < 8; i++) {
    for ( j = 1; j <= max; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = len ? bytes : 0;
      a.off   = b.off   = len ? i     : 0;
      a.bits  = b.bits  = i;
      memset(c, 0x0, len);

      _bsr_bytes_any_slow(&a, j, c);
      ur_bsr_skip_any(&b, j);

      ret &= _bsr_cmp_any_check(cap, i, j, &a, &b);
    }
  }

  free(bytes);
  free(c);

  return ret;
}

static int
_test_bsr_skip_any(void)
{
  return _test_bsr_skip_any_loop("bsr skip nought", 0, 0x0)
       & _test_bsr_skip_any_loop("bsr skip ones odd", 3, 0xff)
       & _test_bsr_skip_any_loop("bsr skip ones even", 4, 0xff)
       & _test_bsr_skip_any_loop("bsr skip zeros odd", 5, 0x0)
       & _test_bsr_skip_any_loop("bsr skip zeros even", 6, 0x0)
       & _test_bsr_skip_any_loop("bsr skip alt 1 odd", 7, 0xaa)
       & _test_bsr_skip_any_loop("bsr skip alt 1 even", 8, 0xaa)
       & _test_bsr_skip_any_loop("bsr skip alt 2 odd", 9, 0x55)
       & _test_bsr_skip_any_loop("bsr skip alt 2 even", 10, 0x55);
}

/*
**  compare the result and state of two reads (that were not permitted
**  to read past the end of the stream).
*/
static int
_bsr_cmp_check(const char* cap,
               uint8_t     off,
               uint8_t     len,
               ur_bsr_t     *a,
               ur_bsr_t     *b,
               uint8_t       c,
               uint8_t       d,
               ur_cue_res_e  e,
               ur_cue_res_e  f)
{
  int ret = 1;

  if ( !ur_bsr_sane(a) ) {
    fprintf(stderr, "%s: off %u, len %u a insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len, a->off, a->left, a->bits);
    ret = 0;
  }

  if ( !ur_bsr_sane(b) ) {
    fprintf(stderr, "%s: off %u, len %u a insane off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len, b->off, b->left, b->bits);
    ret = 0;
  }

  if ( e != f ) {
    fprintf(stderr, "%s: off %u, len %u ret not equal (%s, %s) off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len,
                    (ur_cue_good == e) ? "good" : "gone",
                    (ur_cue_good == f) ? "good" : "gone",
                    b->off, b->left, b->bits);
    ret = 0;
  }

  if ( (ur_cue_good == e) && (c != d) ) {
    fprintf(stderr, "%s: off %u, len %u val not equal (%02x, %02x) off=%u left=%" PRIu64 " bits=%" PRIu64 "\r\n",
                    cap, off, len, c, d, b->off, b->left, b->bits);
    ret = 0;
  }

  if ( a->off != b->off ) {
    fprintf(stderr, "%s: off %u len %u: offset fail (%u, %u)\r\n",
                    cap, off, len, a->off, b->off);
    ret = 0;
  }

  if ( a->left != b->left ) {
    fprintf(stderr, "%s: off %u len %u: left fail (%" PRIu64 ", %" PRIu64 ")\r\n",
                    cap, off, len, a->left, b->left);
    ret = 0;
  }

  if ( a->bits != b->bits ) {
    fprintf(stderr, "%s: off %u len %u: bits fail (%" PRIu64 ", %" PRIu64 ")\r\n",
                    cap, off, len, a->bits, b->bits);
    ret = 0;
  }

  if ( a->bytes != b->bytes ) {
    fprintf(stderr, "%s: off %u len %u: bytes fail (%p, %p)\r\n",
                    cap, off, len, a->bytes, b->bytes);
    ret = 0;
  }


  return ret;
}

/*
**  ur_bsr_log golden master
*/
static ur_cue_res_e
_bsr_log_slow(ur_bsr_t *bsr, uint8_t *out)
{
  ur_cue_res_e res;
  uint8_t   bit, i = 0;

  do {
    if ( ur_cue_good != (res = ur_bsr_bit(bsr, &bit)) ) {
      return res;
    }
    else if ( bit ) {
      *out = i;
      return ur_cue_good;
    }
  }
  while ( ++i );

  return ur_cue_meme;
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a varying numbers of bits via ur_bsr_log and master, comparing
**  the results and respective states each time.
*/
static int
_test_bsr_log_loop(const char *cap, uint8_t len, uint8_t val)
{
  int          ret = 1;
  ur_bsr_t    a, b;
  uint8_t    *bytes, c, d;
  uint8_t     i, j;
  ur_cue_res_e   e, f;

  bytes = malloc(len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j < len; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = bytes;
      a.off = a.bits = b.off = b.bits = i;

      memset(bytes, 0x0, j);
      memset(bytes + j, val, len - j);

      e = _bsr_log_slow(&a, &c);
      f = ur_bsr_log(&b, &d);

      ret &= _bsr_cmp_check(cap, i, j, &a, &b, c, d, e, f);
    }
  }

  free(bytes);

  return ret;
}

static int
_test_bsr_log(void)
{
  int ret = _test_bsr_log_loop("bsr log nought", 0, 0x0)
          & _test_bsr_log_loop("bsr log ones odd", 3, 0xff)
          & _test_bsr_log_loop("bsr log ones even", 4, 0xff)
          & _test_bsr_log_loop("bsr log ones big", 50, 0xff)
          & _test_bsr_log_loop("bsr log zeros odd", 5, 0x0)
          & _test_bsr_log_loop("bsr log zeros even", 6, 0x0)
          & _test_bsr_log_loop("bsr log zeros big", 50, 0x0);

  {
    uint8_t i, j = 5;
    char  cap[1024];

    for ( i = 0; i < 8; i++ ) {
      snprintf(cap, 1000, "bsr log 1<<%u odd", i);
      ret &= _test_bsr_log_loop((const char*)cap, j++, 0x1 << i);

      snprintf(cap, 1000, "bsr log 1<<%u even", i);
      ret &= _test_bsr_log_loop((const char*)cap, j++, 0x1 << i);

      snprintf(cap, 1000, "bsr log 1<<%u big", i);
      ret &= _test_bsr_log_loop((const char*)cap, 50, 0x1 << i);
    }
  }

  return ret;
}

/*
**  ur_bsr_tag golden master
*/
static ur_cue_res_e
_bsr_tag_slow(ur_bsr_t *bsr, ur_cue_tag_e *out)
{
  ur_cue_res_e res;
  uint8_t      bit;

  if ( ur_cue_good != (res = ur_bsr_bit(bsr, &bit)) ) {
    return res;
  }
  else if ( 0 == bit ) {
    *out = ur_jam_atom;
    return ur_cue_good;
  }
  else if ( ur_cue_good != (res = ur_bsr_bit(bsr, &bit)) ) {
    return res;
  }

  *out = ( 0 == bit ) ? ur_jam_cell : ur_jam_back;
  return ur_cue_good;
}

/*
**  from a bitstream-reader initialized with varying values/lengths/offsets,
**  read a jam type tag via ur_bsr_tag and master, comparing the results and
**  respective states each time.
*/
static int
_test_bsr_tag_loop(const char *cap, uint8_t len, uint8_t val)
{
  int           ret = 1;
  ur_bsr_t     a, b;
  uint8_t    *bytes;
  ur_cue_tag_e c, d;
  uint8_t      i, j;
  ur_cue_res_e    e, f;

  bytes = malloc(len);

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j < len; j++ ) {
      a.left = b.left = len;
      a.bytes = b.bytes = bytes;
      a.off = a.bits = b.off = b.bits = i;

      memset(bytes, 0x0, j);
      memset(bytes + j, val, len - j);

      e = _bsr_tag_slow(&a, &c);
      f = ur_bsr_tag(&b, &d);

      ret &= _bsr_cmp_check(cap, i, j, &a, &b, c, d, e, f);
    }
  }

  free(bytes);

  return ret;
}

static int
_test_bsr_tag(void)
{
  return _test_bsr_tag_loop("bsr tag nought", 0, 0x0)
       & _test_bsr_tag_loop("bsr tag ones 1", 1, 0xff)
       & _test_bsr_tag_loop("bsr tag ones 2", 2, 0xff)
       & _test_bsr_tag_loop("bsr tag zeros 1", 1, 0x0)
       & _test_bsr_tag_loop("bsr tag zeros 2", 2, 0x0)
       & _test_bsr_tag_loop("bsr tag alt-1 1", 1, 0xaa)
       & _test_bsr_tag_loop("bsr tag alt-1 2", 2, 0xaa)
       & _test_bsr_tag_loop("bsr tag alt-2 1", 1, 0x55)
       & _test_bsr_tag_loop("bsr tag alt-2 2", 2, 0x55);
}

static int
_test_bsr(void)
{
  return _test_bsr_bit()
       & _test_bsr_bit_any()
       & _test_bsr_bytes_any()
       & _test_bsr_skip_any()
       & _test_bsr8()
       & _test_bsr32()
       & _test_bsr64()
       & _test_bsr_log()
       & _test_bsr_tag();
}

static int
_test_jam_spec(const char    *cap,
               ur_root_t       *r,
               ur_nref        ref,
               size_t         len,
               const uint8_t *res)
{
  uint64_t   i, out_len;
  uint8_t *out;
  int      ret;

  ur_jam(r, ref, &out_len, &out);

  if ( 0 != memcmp(out, res, len) ) {
    fprintf(stderr, "\033[31mjam %s fail\033[0m\r\n", cap);

    fprintf(stderr, "  actual: { ");
    for ( i = 0; i < out_len; i++ ) {
      fprintf(stderr, "0x%x, ", out[i]);
    }
    fprintf(stderr, "}\r\n");
    fprintf(stderr, "  expect: { ");
    for ( i = 0; i < len; i++ ) {
      fprintf(stderr, "0x%x, ", res[i]);
    }
    fprintf(stderr, "}\r\n");

    ret = 0;
  }
  else {
    ret = 1;
  }

  free(out);

  return ret;
}

static int
_test_cue_spec(const char    *cap,
               ur_root_t*       r,
               ur_nref        ref,
               size_t         len,
               const uint8_t *res)
{
  int     ret = 1;
  ur_nref out;

  if ( !ur_cue_test(len, res) ) {
    fprintf(stderr, "\033[31mcue %s fail 1\033[0m\r\n", cap);
    ret = 0;
  }

  if ( ur_cue_good != ur_cue(r, len, res, &out) ) {
    fprintf(stderr, "\033[31mcue %s fail 2\033[0m\r\n", cap);
    ret = 0;
  }
  else if ( ref != out ) {
    fprintf(stderr, "\033[31mcue %s fail 3 ref=%" PRIu64 " out=%" PRIu64 " \033[0m\r\n", cap, ref, out);
    ret = 0;
  }

  return ret;
}

/*
**  test jam/cue correctness and roundtrips across a variety of inputs
*/
static int
_test_jam_cue(void)
{
  ur_root_t *r = ur_root_init();
  int      ret = 1;

#     define NC(a, b)     ur_cons(r, a, b)
#     define NT(a, b, c)  NC(a, NC(b, c))

#     define FAST         0x74736166
#     define FULL         0x6c6c7566

#     define TEST_CASE(a, b)                                   \
        const char* cap = a;                                   \
        ur_nref     ref = b;                                   \
        ret &= _test_jam_spec(cap, r, ref, sizeof(res), res);  \
        ret &= _test_cue_spec(cap, r, ref, sizeof(res), res);  \

  {
    uint8_t res[1] = { 0x2 };
    TEST_CASE("0", 0);
  }

  {
    uint8_t res[1] = { 0xc };
    TEST_CASE("1", 1);
  }

  {
    uint8_t res[1] = { 0x48 };
    TEST_CASE("2", 2);
  }

  {
    uint8_t res[6] = { 0xc0, 0x37, 0xb, 0x9b, 0xa3, 0x3 };
    TEST_CASE("%fast", FAST);
  }

  {
    uint8_t res[6] = { 0xc0, 0x37, 0xab, 0x63, 0x63, 0x3 };
    TEST_CASE("%full", FULL);
  }

  {
    uint8_t res[1] = { 0x29 };
    TEST_CASE("[0 0]", NC(0, 0));
  }

  {
    uint8_t res[2] = { 0x31, 0x3 };
    TEST_CASE("[1 1]", NC(1, 1));
  }

  {
    uint8_t res[2] = { 0x21, 0xd1 };
    TEST_CASE("[2 3]", NC(2, 3));
  }

  {
    uint8_t res[11] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0xe, 0x7c, 0xb3, 0x3a, 0x36, 0x36 };
    TEST_CASE("[%fast %full]", NC(FAST, FULL));
  }

  {
    uint8_t res[2] = {  0x71, 0xcc };
    TEST_CASE("[1 1 1]", NC(1, NC(1, 1)));
  }

  {
    uint8_t res[12] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0x1e, 0xf0, 0xcd, 0xea, 0xd8, 0xd8, 0x93 };
    TEST_CASE("[%fast %full %fast]", NC(FAST, NC(FULL, FAST)));
  }

  {
    uint8_t res[6] = { 0xa5, 0x35, 0x19, 0xf3, 0x18, 0x5 };
    TEST_CASE("[[0 0] [[0 0] 1 1] 1 1]", NC(NC(0, 0), NC(NC(NC(0, 0), NC(1, 1)), NC(1, 1))));
  }

  {
    uint8_t res[14] = { 0x15, 0x17, 0xb2, 0xd0, 0x85, 0x59, 0xb8, 0x61, 0x87, 0x5f, 0x10, 0x54, 0x55, 0x5 };
    TEST_CASE("deep", NC(NC(NC(1, NC(NC(2, NC(NC(3, NC(NC(4, NC(NT(5, 6, NC(7, NC(NC(8, 0), 0))), 0)), 0)), 0)), 0)), 0), 0));
  }

  {
    uint8_t inp[33] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };
    uint8_t res[35] = { 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8 };
    TEST_CASE("wide", ur_coin_bytes(r, sizeof(inp), inp));
  }

  {
    uint8_t inp[16] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xc, 0xa8, 0xab, 0x60, 0xef, 0x2d, 0xd, 0x0, 0x0, 0x80 };
    uint8_t res[19] = { 0x0, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x18, 0x50, 0x57, 0xc1, 0xde, 0x5b, 0x1a, 0x0, 0x0, 0x0, 0x1 };
    TEST_CASE("date", ur_coin_bytes(r, sizeof(inp), inp));
  }

  ur_root_free(r);

  return ret;
}

static int
_test_ur(void)
{
  int ret = 1;

  if ( !_test_bsw() ) {
    fprintf(stderr, "ur test bsw failed\r\n");
    ret = 0;
  }

  if ( !_test_bsr() ) {
    fprintf(stderr, "ur test bsr failed\r\n");
    ret = 0;
  }

  if ( !_test_jam_cue() ) {
    fprintf(stderr, "ur test jam/cue failed\r\n");
    ret = 0;
  }

  return ret;
}

int
main(int argc, char* argv[])
{
  if ( !_test_ur() ) {
    fprintf(stderr, "ur test failed\r\n");
    return 1;
  }

  fprintf(stderr, "ur ok\n");
  return 0;
}
