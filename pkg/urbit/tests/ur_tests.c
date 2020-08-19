#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "ur/hashcons.h"

static void
_bsw_init(ur_bsw_t *bsw, uint64_t prev, uint64_t size)
{
  bsw->prev = prev;
  bsw->size = size;
  bsw->bits = 0;
  bsw->fill = 0;
  bsw->off  = 0;

  free(bsw->bytes);
  bsw->bytes = calloc(size, 1);
}

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

static int
_test_bsw_bit_ones(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_init(&bsw, 1, 1);

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

static int
_test_bsw_bit_zeros(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_init(&bsw, 1, 1);

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

static int
_test_bsw_bit_alt(void)
{
  int      ret = 1;
  ur_bsw_t bsw = {0};
  _bsw_init(&bsw, 1, 1);

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

  if ( a->bytes[0] != b->bytes[0] ) {
    fprintf(stderr, "%s: val 0x%02x off %u len %u: bytes fail (0x%02x, 0x%02x)\r\n",
                    cap, val, off, len, a->bytes[0], b->bytes[0]);
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

  return ret;
}

static void
_bsw8_slow(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  len = (len > 8) ? 8 : len;

  while ( len ) {
    ur_bsw_bit(bsw, byt);
    byt >>= 1;
    len--;
  }
}

static int
_test_bsw8_loop(const char* cap, uint8_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 8; j++ ) {
      _bsw_init(&a, 1, 1);
      _bsw_init(&b, 1, 1);
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

static void
_bsw32_slow(ur_bsw_t *bsw, uint8_t len, uint32_t val)
{
  len = (len > 32) ? 32 : len;

  while ( len ) {
    ur_bsw_bit(bsw, val & 0xff);
    val >>= 1;
    len--;
  }
}

static int
_test_bsw32_loop(const char* cap, uint32_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 32; j++ ) {
      _bsw_init(&a, 1, 1);
      _bsw_init(&b, 1, 1);
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

static void
_bsw64_slow(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  len = (len > 64) ? 64 : len;

  while ( len ) {
    ur_bsw_bit(bsw, val & 0xff);
    val >>= 1;
    len--;
  }
}

static int
_test_bsw64_loop(const char* cap, uint64_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j;

  for ( i = 0; i < 8; i++) {
    for ( j = 0; j <= 64; j++ ) {
      _bsw_init(&a, 1, 1);
      _bsw_init(&b, 1, 1);
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

static int
_test_bsw_bytes_loop(const char* cap, uint64_t len, uint8_t val)
{
  int    ret = 1;
  ur_bsw_t a = {0};
  ur_bsw_t b = {0};
  uint8_t i, j, *byt;

  for ( i = 0; i < 8; i++) {
    _bsw_init(&a, 1, 1);
    _bsw_init(&b, 1, 1);
    a.off = a.bits = b.off = b.bits = i;
    byt = malloc(len);

    for ( j = 0; j < len; j++ ) {
     _bsw8_slow(&a, 8, val);
     byt[j] = val;
    }

    ur_bsw_bytes(&b, len, byt);
    free(byt);

    ret &= _bsw_cmp_check(cap, val, 8, i, &a, &b);
  }

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
       & _test_bsw_bytes_loop("bsw bytes alt 2 odd", 10, 0x55);
}

static int
_test_bsw(void)
{
  return _test_bsw_bit()
       & _test_bsw8()
       & _test_bsw32()
       & _test_bsw64()
       & _test_bsw_bytes();
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
  ur_nref out;

  if ( ur_cue_good != ur_cue(r, len, res, &out) ) {
    fprintf(stderr, "\033[31mcue %s fail 1\033[0m\r\n", cap);
    return 0;
  }
  else if ( ref != out ) {
    fprintf(stderr, "\033[31mcue %s fail 2 ref=%" PRIu64 " out=%" PRIu64 " \033[0m\r\n", cap, ref, out);
    return 0;
  }

  return 1;
}

static int
_test_jam_cue(void)
{
  ur_root_t *r = ur_hcon_init();
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
    TEST_CASE("wide", ur_coin_bytes(r, inp, sizeof(inp)));
  }

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
