#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "ur/hashcons.h"

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
